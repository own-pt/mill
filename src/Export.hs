{-# LANGUAGE StrictData #-}
module Export where

import Data ( WNid(..), WNExtra(..), LexicographerFileId(..)
            , WordSenseId(..), SynsetId(..), Synset(..), Validated
            , SynsetRelation(..), WSense(..), WordPointer(..)
            , lexicographerFileIdToText, senseKey, singleton, synsetId, synsetType
            , tshow, extraFrames, showId
            , WNPOS(..), unsafeLookup, WordSenseForm(..), LexicalId(..) )
import Validate (DupsIndex, Index, indexKey, lookupIndex, getSynset)
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder,charUtf8)
import Data.CaseInsensitive ( foldCase )
import Data.Coerce (coerce)
import Data.List (findIndex, mapAccumL, nub, sort)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.ListTrie.Patricia.Map (foldlWithKey', empty, insertWith', lookupPrefix, foldlDescWithKey')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)
import Prelude hiding (lookup)


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

---
-- JSON/aeson
synsetToJSON :: Index (Synset a) -> Map Text Text -> Map Text Int
  -> Synset Validated -> Value
synsetToJSON index textToCanonicNames _
  Synset{comments, wordSenses = wordSenses@(WSense headWordId@(WordSenseId _) _ _:|_), ..}
  = object $ concat
    [ synsetFrames extra
    , if null comments then [] else ["_comments" .= comments]
    , if null examples then [] else ["examples" .= examples]
    , if null relations then [] else ["relations"  .= map synsetRel relations]
    , [ "id"         .= showId (coerce headWordId)
      , "wordsenses" .= NE.map toWordSense wordSenses
      , "definition" .= definition
      , "_position"   .= sourcePosition
      ]
    ]
  where
    synsetRel (SynsetRelation name targetId@(SynsetId WNid{lexForm})) =
      object [ "name" .= name
             , "targetId" .= targetSynsetId
             , "_targetLexicalForm" .= lexForm
             ]
      where
        targetSynsetId = synsetId . getSynset index $ coerce targetId
    synsetFrames (WNVerb frames) = ["frames" .= NE.toList frames]
    synsetFrames _ = []
    toRelation name wnIdentifier = object ["name" .= unsafeLookup (missingRelation name) name textToCanonicNames, "id" .= showId wnIdentifier]
    missingRelation name = "No relation with name " ++ show name ++ " found in relation.tsv"
    toWordSense (WSense (WordSenseId WNid{lexForm, lexId}) wExtra pointers)
      = object $ concat
      [ if null pointers then [] else ["pointers" .= senseRels]
      , wordExtra wExtra
      , [ "lexicalForm" .= lexForm
        , "lexicalId" .= lexId
        ]
      ]
      where
        wordExtra (WNAdj marker)  = ["syntacticMarker" .= marker]
        wordExtra (WNVerb frames) = ["frames" .= NE.toList frames]
        wordExtra _ = []
        senseRels =
          map (\(WordPointer name targetIdentifier) -> toRelation name $ coerce targetIdentifier)
          pointers

synsetsToSynsetJSONs :: Index (Synset a) -> Map Text Text -> Map Text Int
  -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs index textToCanonicNames lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON index textToCanonicNames lexNamesToLexNum) synsets


---
-- WNDB

-- first we convert synsets to this structure which has all the info
-- we need
data DBSynset = DBSynset
  { _id                  :: WNid
  , lexicographerFileNum :: Int
  , pos                  :: WNPOS
  , wordSenses           :: NonEmpty (Text, Int)
  , gloss                :: Text
  , frames               :: [(Int,Int)]
  , relations            :: [(Text, SynsetId, WNPOS, (Int, Int))]
  } deriving (Show,Eq)

synsetToDB :: Map Text Text -> Map Text Int -> Index (Synset a) -> Synset Validated -> DBSynset
synsetToDB relationsMap lexicographerMap index
  Synset{lexicographerFileId, wordSenses = wordSenses@(WSense (WordSenseId headId@WNid{pos}) _ _:|_), definition, examples, extra, relations} =
  DBSynset { _id = headId
           , lexicographerFileNum = let lexname = lexicographerFileIdToText lexicographerFileId
                                    in unsafeLookup
                                       ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
                                       lexname
                                       lexicographerMap
           , pos = pos
           , wordSenses = NE.map toWord wordSenses -- FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition
                                         : map (\e -> T.cons '"' $ T.snoc e '"') examples)
           , frames = map synsetFrame (extraFrames extra)
                    ++ concatMap toWordFrames (zip [1..] $ NE.toList wordSenses)
           , relations = map synsetRelation relations
                         ++ concatMap wordRelations (zip [1..] $ NE.toList wordSenses)
           }
  where
    toWord (WSense (WordSenseId WNid{lexForm = WordSenseForm form,lexId = LexicalId lexicalId}) _ _) = (form, lexicalId)
    synsetRelation (SynsetRelation relationName
                    wnIdentifier@(SynsetId WNid{pos=wnPoS}))
      = ( unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsMap
        , wnIdentifier, wnPoS, (0,0))
    synsetFrame = (0,)
    toWordFrames (ix, WSense{extra = wordExtra}) = map (ix,) $ extraFrames wordExtra
    wordRelations (ix, WSense _ _ wordPointers) =
      map (wordRelation ix) wordPointers
    wordRelation ix (WordPointer pointerName
                       (WordSenseId wnId@WNid{pos=wnPoS})) =
      ( unsafeLookup ("Missing pointer " ++ T.unpack pointerName ++ "in relations.tsv") pointerName relationsMap
      , synsetId . fromMaybe (error "Can't find key in index") $ lookupIndex (indexKey wnId) index
      , wnPoS
      , (ix, getWordSenseIndex wnId))
    getSynsetWords wnId =
      let sensekey = indexKey wnId
      in case lookupIndex sensekey index of
        Just Synset{wordSenses = synsetWords} -> NE.toList synsetWords
        Nothing -> error $ "No synset corresponding to sense key " ++ sensekey
    getWordSenseIndex wnId =
      let synsetWords = getSynsetWords wnId
      in fromMaybe
           (error $ "No wordsense corresponding to word sense " ++ show wnId)
           $ findIndex (\(WSense (WordSenseId synsetWnId) _ _) -> synsetWnId == wnId) synsetWords

padText :: Int -> Text -> Text
padText n x = T.append (T.replicate (n - T.length x) "0") x

padNum :: Int -> Int -> Text
padNum n m = let x = T.pack $ show m in padText n x

wnIdentifierToOffset :: OffsetMap -> WNid -> Text
wnIdentifierToOffset offsetMap wnId = padNum 8 $ M.findWithDefault 0 (indexKey wnId) offsetMap

showHumanPoS :: WNPOS -> Text
-- shows S as "a" instead of "s"
showHumanPoS N = "n"
showHumanPoS V = "v"
showHumanPoS S = "a"
showHumanPoS A = "a"
showHumanPoS R = "r"

-- render a DBSynset; this is done twice for each synset, the first
-- one has no offset data and is used to calculate it. in the second
-- run we can use the offsets. this is not optimized at all, but
-- performance seems acceptable, so who cares.
showDBSynset :: OffsetMap -> DBSynset -> Text
showDBSynset offsetMap DBSynset{ _id, lexicographerFileNum, pos, wordSenses, frames, relations, gloss}
  = T.unwords
  [ offsetDoc
  , padNum 2 lexicographerFileNum
  , showHumanPoS pos
  , wordCount
  , T.unwords . NE.toList $ NE.map synsetWord wordSenses
  , T.unwords $ pointerCount : map synsetPointer relations
  , if pos == V then T.unwords $ padNum 2 (length frames) : map synsetFrame frames else ""
  , "|", gloss]
  where
    pointerCount = padNum 3 $ length relations
    wordCount = paddedHex 2 $ NE.length wordSenses
    idOffset = wnIdentifierToOffset offsetMap
    offsetDoc = idOffset _id 
    paddedHex n h = padText n . T.pack $ showHex h ""
    synsetWord (wordForm, lexId) = T.unwords [T.replace " " "_" wordForm, T.pack $ showHex lexId ""]
    synsetPointer (pointerSym, SynsetId targetId, targetPos, (sourceNum,targetNum))
      = T.unwords [ pointerSym, idOffset targetId, showHumanPoS targetPos
                  , paddedHex 2 sourceNum <> paddedHex 2 targetNum ]
    synsetFrame (frameNum, wordNum) = padNum 2 frameNum <> " " <> paddedHex 2 wordNum

newline :: Text
newline = "\n"

calculateOffsets :: Int -> OffsetMap -> Map Text Text -> Map Text Int -> Index (Synset a) -> NonEmpty (Synset Validated)
  -> (OffsetMap, NonEmpty DBSynset)
calculateOffsets startOffset startOffsetMap relationsMap lexicographerMap index synsets
  = (\((offsetMap, _), dbSynsets) -> (offsetMap, dbSynsets)) $ mapAccumL go (startOffsetMap, startOffset) synsets
  where
    getBytesize = B.length . encodeUtf8
    newlineSize = getBytesize newline
    go (offsetMap, offset) synset =
      let dbSynset@DBSynset{_id} = synsetToDB relationsMap lexicographerMap index synset
          increment = getBytesize $ showDBSynset M.empty dbSynset
      in ( (M.insert (indexKey _id) offset offsetMap, offset + increment + newlineSize)
         , dbSynset
         )

type WNDBindex a = DupsIndex a
type OffsetMap = Map String Int

-- this depends heavily on the indexkey
makeWndbIndex :: Index a -> WNDBindex a
-- make index for index* files (which see
-- https://wordnet.princeton.edu/documentation/wndb5wn) contain case
-- insensitive lemmas
makeWndbIndex = foldlWithKey' go empty
  where
    go :: String -> Either String a -> WNDBindex a -> WNDBindex a
    go key value = insertWith' (<>) (newKey key) $ singleton value
    newKey key = let (lemma, rest) = break (=='\t') . tail $ dropWhile (/='\t') key
                     pos = read . take 1 $ drop 1 rest
                 in concat [show (if pos == S then A else pos), "\t", foldCase lemma]


--- lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...]
showIndex :: WNPOS -> Map Text Text -> OffsetMap -> Index (Synset a) -> DupsIndex (Synset a) -> [Text]
showIndex wnPos relationsLexName offsetMap index wndbIndex = foldlDescWithKey' go [] posIndex
  where
    go key synsetsOrRefs resultSoFar = line key synsetsOrRefs : resultSoFar
    line key synsetsOrRefs =
        T.unwords [ lemma
                  , pos
                  , synsetCount
                  , T.unwords $ tshow (length lemmaRelations) : map toLexRelationName lemmaRelations
                  , synsetCount
                  , "0" -- FIXME: count of tags of sense in corpus (not implemented)
                  , T.unwords . NE.toList $ NE.map synsetOffset synsets
                  ]
      where
        lemma = T.pack . drop 1 $ dropWhile (/= '\t') key
        synsetCount = tshow $ length synsetsOrRefs
        synsets = NE.map toSynset synsetsOrRefs
        lemmaRelations = nub $ concatMap (wordRelations lemma) synsets
    wordRelations lemma Synset{relations, wordSenses} = concatMap lemmaPointers wordSenses ++ map (\(SynsetRelation name _) -> name) relations
      where
        lemmaPointers (WSense (WordSenseId WNid{lexForm=WordSenseForm wordForm}) _ wordPointers)
          = if foldCase wordForm == lemma then map pointerName wordPointers else []
    pointerName (WordPointer name _) = name
    toLexRelationName relationName = unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsLexName
    synsetOffset = wnIdentifierToOffset offsetMap . (\(SynsetId wnIdentifier) -> wnIdentifier) . synsetId
    toSynset (Left headKey)
      = fromMaybe (error $ "Missing key " ++ headKey ++ " in index") $ lookupIndex headKey index
    toSynset (Right synset) = synset
    posIndex = lookupPrefix (T.unpack $ T.toUpper pos) wndbIndex
    pos = showHumanPoS wnPos

synsetLexFileNum :: Map Text Int -> Synset a -> Int
synsetLexFileNum lexMap Synset{lexicographerFileId}
  =
  let lexname = lexicographerFileIdToText lexicographerFileId
  in unsafeLookup
     ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
     lexname
     lexMap

wndbSenseIndex :: Map Text Int -> Index (Synset a) -> OffsetMap -> [Text]
wndbSenseIndex lexicographerMap index offsetMap
  = sort $ foldlDescWithKey' go [] index
  where
    go _ (Left _) resultSoFar = resultSoFar
    go _ (Right synset@Synset{wordSenses}) resultSoFar =
      zipWith line (NE.toList wordSenses) ([1..] :: [Int])
      ++ resultSoFar
      where
        synsetID = coerce $ synsetId synset
        synsetPOS = pos (lexicographerFileId synset :: LexicographerFileId)
        lexFileNum = synsetLexFileNum lexicographerMap synset
        synsetTypeNum = synsetType synsetPOS
        -- hardcoded ALERT
        isHeadRel (SynsetRelation "sim" _) = True
        isHeadRel _ = False
        maybeHeadRel =
          case synsetPOS of
            S -> case filter isHeadRel $ Data.relations synset of
                   [headRel] -> Just headRel
                   _ -> error "No head or more than one head"
            _ -> Nothing
        line sense n =
          T.unwords [ T.pack sensekey -- sensekey
                    , wnIdentifierToOffset offsetMap synsetID -- offset
                    , tshow n -- sense number (faked)
                    , "0" -- FIXME: count tags of sense in corpus
                    ]
          where
            sensekey = senseKey lexFileNum synsetTypeNum maybeHeadRel sense

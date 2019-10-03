{-# LANGUAGE StrictData #-}
module Export where

import Data ( LexicographerFileId(..)
            , WordSenseIdentifier(..), SynsetIdentifier(..), Synset(..), Validated
            , SynsetRelation(..), WNWord(..), WordPointer(..)
            , lexicographerFileIdToText, senseKey, singleton, synsetId, synsetType
            , tshow
            , WNPOS(..), unsafeLookup, WordSenseForm(..), LexicalId(..) )
import Validate (DupsIndex, Index, indexKey, lookupIndex)
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder,charUtf8)
import Data.CaseInsensitive ( foldCase )
import Data.List (find, findIndex, mapAccumL, nub)
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

unUnderline :: WordSenseForm -> WordSenseForm
-- | substitutes underlines for spaces in wordsense form
unUnderline (WordSenseForm form) = WordSenseForm $ T.replace "_" " " form

---
-- JSON/aeson
synsetToJSON :: Map Text Text -> Map Text Int -> Synset Validated -> Value
synsetToJSON textToCanonicNames lexNamesToLexNum
  Synset{wordSenses = wordSenses@(WNWord headWordId@(WordSenseIdentifier (lexFileId@(LexicographerFileId (wnPOS, _)), _, _)) _ _:|_), ..}
  = object
  [ "id"         .= headWordId
  , "wordsenses" .= NE.map toWordSense wordSenses
  , "definition" .= definition
  , "examples"   .= examples
  , "frames"     .= frames
  , "relations"  .= map (\(SynsetRelation name targetIdentifier) -> toRelation name targetIdentifier) relations
  , "position"   .= sourcePosition
  ]
  where
    toRelation name wnIdentifier = object ["name" .= unsafeLookup (missingRelation name) name textToCanonicNames, "id" .= wnIdentifier]
    missingRelation name = "No relation with name " ++ show name ++ " found in relation.tsv"
    toWordSense wordSense@(WNWord (WordSenseIdentifier (_, lexForm, lexId)) wordFrames pointers)
      = object
      [ "lexicalForm" .= unUnderline lexForm
      , "lexicalId" .= lexId
      , "frames" .= wordFrames
      , "pointers" .= map (\(WordPointer name targetIdentifier) -> toRelation name targetIdentifier) pointers
      , "senseKey" .= senseKey lexFileNum (synsetType wnPOS) maybeHeadSynset wordSense
      ]
    lexfileName  = lexicographerFileIdToText lexFileId
    lexFileNum = unsafeLookup ("No lexfile with name "
                               ++ show lexfileName ++ "found in lexnames.tsv")
                 lexfileName lexNamesToLexNum
    isHeadRelation (SynsetRelation "sim" _) = True
    isHeadRelation _ = False
    maybeHeadSynset = case wnPOS of
      S -> find isHeadRelation relations
      _ -> Nothing

synsetsToSynsetJSONs :: Map Text Text -> Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs textToCanonicNames lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON textToCanonicNames lexNamesToLexNum) synsets


---
-- WNDB

-- first we convert synsets to this structure which has all the info
-- we need
data DBSynset = DBSynset
  { _id             :: (LexicographerFileId, WordSenseForm, LexicalId)
  , lexicographerFileNum :: Int
  , pos                  :: WNPOS
  , wordSenses           :: NonEmpty (Text, Int)
  , gloss                :: Text
  , frames               :: [(Int,Int)]
  , relations            :: [(Text, SynsetIdentifier, WNPOS, (Int, Int))]
  } deriving (Show,Eq)

synsetToDB :: Map Text Text -> Map Text Int -> Index (Synset a) -> Synset Validated -> DBSynset
synsetToDB relationsMap lexicographerMap index
  Synset{lexicographerFileId, wordSenses = wordSenses@(WNWord (WordSenseIdentifier headId) _ _:|_), definition, examples, frames, relations} =
  DBSynset { _id = headId
           , lexicographerFileNum = let lexname = lexicographerFileIdToText lexicographerFileId
                                    in unsafeLookup
                                       ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
                                       lexname
                                       lexicographerMap
           , pos = (\(LexicographerFileId (wnPos,_)) -> wnPos) lexicographerFileId
           , wordSenses = NE.map toWord wordSenses -- FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition
                                         : map (\e -> T.cons '"' $ T.snoc e '"') examples)
           , frames = map synsetFrame frames
                    ++ concatMap toWordFrames (zip [1..] $ NE.toList wordSenses)
           , relations = map synsetRelation relations
                         ++ concatMap wordRelations (zip [1..] $ NE.toList wordSenses)
           }
  where
    toWord (WNWord (WordSenseIdentifier (_,WordSenseForm form,LexicalId lexId)) _ _) = (form, lexId)
    synsetRelation (SynsetRelation relationName
                    wnIdentifier@(SynsetIdentifier (LexicographerFileId (wnPos,_),_,_)))
      = ( unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsMap
        , wnIdentifier, wnPos, (0,0))
    synsetFrame = (0,)
    toWordFrames (ix, WNWord _ wordFrames _) = map (ix,) wordFrames
    wordRelations (ix, WNWord _ _ wordPointers) =
      map (wordRelation ix) wordPointers
    wordRelation ix (WordPointer pointerName
                       (WordSenseIdentifier wnIdentifier@(LexicographerFileId
                                                          (wnPos,_), _, _))) =
      ( unsafeLookup ("Missing pointer " ++ T.unpack pointerName ++ "in relations.tsv") pointerName relationsMap
      , synsetId . fromMaybe (error "Can't find key in index") $ lookupIndex (uncurry3 indexKey wnIdentifier) index
      , wnPos
      , (ix, getWordSenseIndex wnIdentifier))
    getSynsetWords (lexFileId,wordSenseForm,lexId) =
      let sensekey = indexKey lexFileId wordSenseForm lexId
      in case lookupIndex sensekey index of
        Just Synset{wordSenses = synsetWords} -> NE.toList synsetWords
        Nothing -> error $ "No synset corresponding to sense key " ++ sensekey
    getWordSenseIndex wnIdentifier =
      let synsetWords = getSynsetWords wnIdentifier
      in fromMaybe
           (error $ "No wordsense corresponding to word sense " ++ show wnIdentifier)
           $ findIndex (\(WNWord (WordSenseIdentifier wnId) _ _) -> wnId == wnIdentifier) synsetWords

padText :: Int -> Text -> Text
padText n x = T.append (T.replicate (n - T.length x) "0") x

padNum :: Int -> Int -> Text
padNum n m = let x = T.pack $ show m in padText n x

wnIdentifierToOffset :: Map String Int -> (LexicographerFileId, WordSenseForm, LexicalId) -> Text
wnIdentifierToOffset offsetMap (lexFile, lexForm, lexId) = padNum 8 $ M.findWithDefault 0 (indexKey lexFile lexForm lexId) offsetMap

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
showDBSynset :: Map String Int -> DBSynset -> Text
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
    synsetPointer (pointerSym, SynsetIdentifier targetId, targetPos, (sourceNum,targetNum))
      = T.unwords [ pointerSym, idOffset targetId, showHumanPoS targetPos
                  , paddedHex 2 sourceNum <> paddedHex 2 targetNum ]
    synsetFrame (frameNum, wordNum) = padNum 2 frameNum <> " " <> paddedHex 2 wordNum

newline :: Text
newline = "\n"

calculateOffsets :: Int -> Map String Int -> Map Text Text -> Map Text Int -> Index (Synset a) -> NonEmpty (Synset Validated)
  -> (Map String Int, NonEmpty DBSynset)
calculateOffsets startOffset startOffsetMap relationsMap lexicographerMap index synsets
  = (\((offsetMap, _), dbSynsets) -> (offsetMap, dbSynsets)) $ mapAccumL go (startOffsetMap, startOffset) synsets
  where
    getBytesize = B.length . encodeUtf8
    newlineSize = getBytesize newline
    go (offsetMap, offset) synset =
      let dbSynset@DBSynset{_id = (lexFile, lexForm, lexId)} = synsetToDB relationsMap lexicographerMap index synset
          increment = getBytesize $ showDBSynset M.empty dbSynset
      in ( (M.insert (indexKey lexFile lexForm lexId) offset offsetMap, offset + increment + newlineSize)
         , dbSynset
         )


-- this depends heavily on the indexkey
makeIndexIndex :: Index a -> DupsIndex a
-- make index for index* files (which see
-- https://wordnet.princeton.edu/documentation/wndb5wn) contain case
-- insensitive lemmas
makeIndexIndex = foldlWithKey' go empty
  where
    go :: String -> Either String a -> DupsIndex a -> DupsIndex a
    go key value = insertWith' (<>) (newKey key) $ singleton value
    newKey key = let (lemma, rest) = break (=='\t') key
                     pos = read . take 1 $ drop 1 rest
                 in concat [show (if pos == S then A else pos), "\t", foldCase lemma]


--- lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...]
showIndex :: WNPOS -> Map Text Text -> Map String Int -> Index (Synset a) -> DupsIndex (Synset a) -> [Text]
showIndex wnPos relationsLexName offsetMap index indexIndex = foldlDescWithKey' go [] posIndex
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
        lemmaPointers (WNWord (WordSenseIdentifier (_,WordSenseForm wordForm,_)) _ wordPointers)
          = if foldCase wordForm == lemma then map pointerName wordPointers else []
    pointerName (WordPointer name _) = name
    toLexRelationName relationName = unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsLexName
    synsetOffset = wnIdentifierToOffset offsetMap . (\(SynsetIdentifier wnIdentifier) -> wnIdentifier) . synsetId
    toSynset (Left headKey)
      = fromMaybe (error $ "Missing key " ++ headKey ++ " in index") $ lookupIndex headKey index
    toSynset (Right synset) = synset
    posIndex = lookupPrefix (T.unpack $ T.toUpper pos) indexIndex
    pos = showHumanPoS wnPos

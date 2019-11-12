{-# LANGUAGE StrictData #-}
module Export where

import Data ( WNid(..), WNExtra(..), Relation(..), SynsetId(..)
            , WordSenseId(..), Synset(..), Validated, WNPOS(..)
            , SynsetRelation(..), WSense(..), WordPointer(..)
            , unsafeLookup
            , lexicographerFileIdToText, singleton, synsetId
            , tshow, extraFrames, WordSenseForm(..)
            )
import Validate (ValIndex, SynsetMap, indexKey, lookupIndex
                , lookupSynset, lookupSense, wordSenseKey)
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder,charUtf8)
import Data.CaseInsensitive ( foldCase )
import Data.Coerce (coerce)
import Data.List (findIndex, mapAccumL, nub)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.ListTrie.Patricia.Map (lookupPrefix, foldlDescWithKey', fromListWith')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust) -- oh, the pain
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)
import Prelude hiding (lookup)


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

---
-- JSON/aeson
synsetToJSON :: Map Text Text -> Map Text Int -> Synset Validated -> Value
synsetToJSON textToCanonicNames _
  Synset{comments, wordSenses = wordSenses@(WSense headWordId@(WordSenseId _) _ _:|_), ..}
  = object
    $ synsetFrames extra
    ++
    [ "id"         .= headWordId
    , "wordsenses" .= NE.map toWordSense wordSenses
    , "definition" .= definition
    , "examples"   .= examples
    , "relations"  .= map (\(SynsetRelation (Relation name targetIdentifier)) -> toRelation name targetIdentifier) relations
    , "position"   .= sourcePosition
    , "comments"   .= comments
    ]
  where
    synsetFrames (WNVerb frames) = ["frames" .= NE.toList frames]
    synsetFrames _ = []
    toRelation name wnIdentifier = object ["name" .= unsafeLookup (missingRelation name) name textToCanonicNames, "id" .= wnIdentifier]
    missingRelation name = "No relation with name " ++ show name ++ " found in relation.tsv"
    toWordSense (WSense (WordSenseId WNid{lexForm}) wExtra pointers)
      = object $
        wordExtra wExtra
        ++ [ "lexicalForm" .= lexForm
           , "pointers" .= map (\(WordPointer (Relation name targetIdentifier)) -> toRelation name targetIdentifier) pointers
           ]
      where
        wordExtra (WNAdj marker)  = ["syntacticMarker" .= marker]
        wordExtra (WNVerb frames) = ["frames" .= NE.toList frames]
        wordExtra _ = []


synsetsToSynsetJSONs :: Map Text Text -> Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs textToCanonicNames lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON textToCanonicNames lexNamesToLexNum) synsets


---
-- WNDB

-- first we convert synsets to this structure which has all the info
-- we need
data DBSynset = DBSynset
  { _id                  :: SynsetId
  , lexicographerFileNum :: Int
  , pos                  :: WNPOS
  , wordSenses           :: NonEmpty (Text, Int)
  , gloss                :: Text
  , frames               :: [(Int,Int)]
  , relations            :: [(Text, SynsetId, WNPOS, (Int, Int))]
  } deriving (Show,Eq)

synsetToDB :: Map Text Text -> Map Text Int -> ValIndex -> SynsetMap Validated -> Synset Validated -> DBSynset
synsetToDB relationsMap lexicographerMap index synsetMap
  Synset{lexicographerFileId, wordSenses = senses@(WSense (WordSenseId headId@WNid{pos}) _ _:|_), sourcePosition, definition, examples, extra, relations} =
  DBSynset { _id = SynsetId headId
           , lexicographerFileNum = let lexname = lexicographerFileIdToText lexicographerFileId
                                    in unsafeLookup
                                       ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
                                       lexname
                                       lexicographerMap
           , pos = pos
           , wordSenses = NE.map toWord senses -- [] FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition
                                         : map (\e -> T.cons '"' $ T.snoc e '"') examples)
           , frames = map synsetFrame (extraFrames extra)
                    ++ concatMap toWordFrames (zip [1..] $ NE.toList senses)
           , relations = map synsetRelation relations
                         ++ concatMap wordRelations (zip [1..] $ NE.toList senses)
           }
  where
    toWord sense@(WSense (WordSenseId WNid{lexForm = WordSenseForm form}) _ _) =
      let wordKey = wordSenseKey sense
          synsets = lookupIndex wordKey index synsetMap
          -- we use as lexicalId the index of synset under the key
          -- corresponding to the sense
          lexicalId = findIndex (\Synset{sourcePosition = sourcePos, lexicographerFileId = lexFileId} -> sourcePosition == sourcePos && lexFileId == lexicographerFileId) synsets
      in (form, fromJust lexicalId)
    synsetRelation (SynsetRelation (Relation relationName
                    wnIdentifier@WNid{pos=wnPoS}))
      = ( unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsMap
        , SynsetId wnIdentifier, wnPoS, (0,0))
    synsetFrame = (0,)
    toWordFrames (ix, WSense{extra = wordExtra}) = map (ix,) $ extraFrames wordExtra
    wordRelations (ix, WSense _ _ wordPointers) =
      map (wordRelation ix) wordPointers
    wordRelation ix (WordPointer (Relation pointerName
                       wnId@WNid{pos=wnPoS})) =
      ( unsafeLookup ("Missing pointer " ++ T.unpack pointerName ++ "in relations.tsv") pointerName relationsMap
      , synsetId targetSynset
      , wnPoS
      , (ix, getWordSenseIndex wnId targetSynset))
      where
        targetSynset = lookupSense (WordSenseId wnId) index synsetMap
        getWordSenseIndex targetSenseId Synset{wordSenses} =
          fromMaybe
          (error $ "No wordsense corresponding to word sense " ++ show targetSenseId ++ " among " ++ show wordSenses)
          . findIndex (\(WSense (WordSenseId senseId) _ _) -> senseId == targetSenseId)
          $ NE.toList wordSenses

padText :: Int -> Text -> Text
padText n x = T.append (T.replicate (n - T.length x) "0") x

padNum :: Int -> Int -> Text
padNum n m = let x = T.pack $ show m in padText n x

wnIdentifierToOffset :: Map String Int -> SynsetId -> Text
wnIdentifierToOffset offsetMap (SynsetId wnId) =
  padNum 8 $ M.findWithDefault 0 (indexKey wnId) offsetMap

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
    idOffset = wnIdentifierToOffset offsetMap . coerce
    offsetDoc = idOffset _id 
    paddedHex n h = padText n . T.pack $ showHex h ""
    synsetWord (wordForm, lexId) = T.unwords [T.replace " " "_" wordForm, T.pack $ showHex lexId ""]
    synsetPointer (pointerSym, targetId, targetPos, (sourceNum,targetNum))
      = T.unwords [ pointerSym, idOffset targetId, showHumanPoS targetPos
                  , paddedHex 2 sourceNum <> paddedHex 2 targetNum ]
    synsetFrame (frameNum, wordNum) = padNum 2 frameNum <> " " <> paddedHex 2 wordNum

newline :: Text
newline = "\n"

calculateOffsets :: Int -> Map String Int -> Map Text Text -> Map Text Int -> ValIndex -> SynsetMap Validated -> NonEmpty (Synset Validated)
  -> (Map String Int, NonEmpty DBSynset)
calculateOffsets startOffset startOffsetMap relationsMap lexicographerMap index synsetMap synsets
  = (\((offsetMap, _), dbSynsets) -> (offsetMap, dbSynsets)) $ mapAccumL go (startOffsetMap, startOffset) synsets
  where
    getBytesize = B.length . encodeUtf8
    newlineSize = getBytesize newline
    go (offsetMap, offset) synset =
      let dbSynset@DBSynset{_id} = synsetToDB relationsMap lexicographerMap index synsetMap synset
          increment = getBytesize $ showDBSynset M.empty dbSynset
      in ( (M.insert (indexKey $ coerce _id) offset offsetMap, offset + increment + newlineSize)
         , dbSynset
         )


-- WARNING: this depends heavily on the indexkey
--- probably better to just create index from synsetmap instead of
--- having this hidden dependency
wndbIndex :: SynsetMap a -> ValIndex
-- make index for index* files (which see
-- https://wordnet.princeton.edu/documentation/wndb5wn) contain case
-- insensitive lemmas
wndbIndex synsetMap = fromListWith' (<>) pairs
  where
    pairs = concatMap go $ M.assocs synsetMap
    go (key, Synset{wordSenses}) = NE.toList $ NE.map (posPair key) wordSenses
    posPair synsetID (WSense (WordSenseId WNid{pos, lexForm = WordSenseForm lexForm}) _ _)
      = (concat [show (if pos == S then A else pos), "\t", foldCase $ T.unpack lexForm], singleton synsetID)

--- lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...]
showWNDBindex :: WNPOS -> Map Text Text -> Map String Int -> ValIndex -> SynsetMap a -> [Text]
showWNDBindex wnPos relationsLexName offsetMap wNDBindex synsetMap = foldlDescWithKey' go [] posIndex
  where
    go key synsetIds resultSoFar = line key synsetIds : resultSoFar
    line key synsetIds =
        T.unwords [ lemma
                  , pos
                  , synsetCount
                  , T.unwords $ tshow (length lemmaRelations) : map toLexRelationName lemmaRelations
                  , synsetCount
                  , "0" -- FIXME: count of tags of sense in corpus (not implemented)
                  , T.unwords . NE.toList $ NE.map synsetOffset synsets
                  ]
      where
        -- see wndbIndex, but key is pos\tlemma
        lemma = T.pack . drop 1 $ dropWhile (/= '\t') key
        synsetCount = tshow $ length synsetIds
        synsets = NE.map (synsetMap `lookupSynset`) synsetIds
        lemmaRelations = nub $ concatMap (wordRelations lemma) synsets
    wordRelations lemma Synset{relations, wordSenses} = concatMap lemmaPointers wordSenses ++ map (relationName . coerce) relations
      where
        lemmaPointers (WSense (WordSenseId WNid{lexForm=WordSenseForm wordForm}) _ wordPointers)
          = if foldCase wordForm == lemma then map (relationName . coerce) wordPointers else []
    relationName (Relation name _) = name
    toLexRelationName relName = unsafeLookup ("Missing relation " ++ T.unpack relName ++ " in relations.tsv") relName relationsLexName
    synsetOffset = wnIdentifierToOffset offsetMap . synsetId
    posIndex = lookupPrefix (T.unpack $ T.toUpper pos) wNDBindex
    pos = showHumanPoS wnPos

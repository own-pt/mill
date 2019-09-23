module Export where

import Data ( LexicographerFileId(..)
            , WordSenseIdentifier(..), SynsetIdentifier(..), Synset(..), Validated
            , SynsetRelation(..), WNWord(..), WordPointer(..)
            , lexicographerFileIdToText, senseKey, synsetType
            , WNPOS(..), unsafeLookup, WordSenseForm(..), LexicalId(..) )
import Validate (Index, indexKey, lookupIndex)
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import Data.ByteString.Builder (Builder,charUtf8)
import Data.List (find, findIndex, mapAccumL)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc, (<+>), hsep, pipe, pretty)
import Numeric (showHex)
import Prelude hiding (lookup)

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
      [ "lexicalForm" .= lexForm
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
  { synsetId             :: (LexicographerFileId, WordSenseForm, LexicalId)
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
  DBSynset { synsetId = headId
           , lexicographerFileNum = let lexname = lexicographerFileIdToText lexicographerFileId
                                    in unsafeLookup
                                       ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
                                       lexname
                                       lexicographerMap
           , pos = (\(LexicographerFileId (wnPos,_)) -> wnPos) lexicographerFileId
           , wordSenses = NE.map toWord wordSenses -- FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition : map (\e -> T.cons '"' $ T.snoc e '"') examples) -- FIXME: add quotes
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
      , SynsetIdentifier wnIdentifier
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

-- render a DBSynset; this is done twice for each synset, the first
-- one has no offset data and is used to calculate it. in the second
-- run we can use the offsets
--- FIXME: we can just output Text here, no?
showSynsetDB :: Map String Int -> DBSynset -> Doc a
showSynsetDB offsetMap DBSynset{ synsetId, lexicographerFileNum, pos, wordSenses, frames, relations, gloss}
  = offsetDoc
  <+> pretty lexicographerFileNum
  <+> showPos pos
  <+> wordCount
  <+> hsep (NE.toList $ NE.map synsetWord wordSenses)
  <+> pointerCount <+> hsep (map synsetPointer relations)
  <+> padNum 2 (length frames) <+> hsep (map synsetFrame frames)
  <+> pipe <+> pretty gloss
  where
    pointerCount = padNum 3 $ length relations
    wordCount = pretty $ (showHex $ NE.length wordSenses) ""
    showPos N = "n"
    showPos V = "v"
    showPos S = "s"
    showPos A = "a"
    showPos R = "r"
    idOffset (lexFile, lexForm, lexId) = padNum 8 $ M.findWithDefault 0 (indexKey lexFile lexForm lexId) offsetMap
    offsetDoc = idOffset synsetId
    padNum n m = let x = T.pack $ show m in padText n x
    paddedHex n h = padText n . T.pack $ showHex h ""
    padText n x = pretty $ T.append (T.replicate (n - T.length x) "0") x
    synsetWord (wordForm, lexId) = pretty (T.replace " " "_" wordForm) <+> pretty (showHex lexId "")
    synsetPointer (pointerSym, SynsetIdentifier targetId, targetPos, (sourceNum,targetNum))
      =   pretty pointerSym <+> idOffset targetId <+> showPos targetPos
      <+> paddedHex 2 sourceNum <> paddedHex 2 targetNum
    synsetFrame (frameNum, wordNum) = padNum 2 frameNum <+> paddedHex 2 wordNum

makeFauxDBs :: Map Text Text -> Map Text Int -> Index (Synset a) -> [Synset Validated]
  -> (Map String Int, [DBSynset])
makeFauxDBs relationsMap lexicographerMap index = mapAccumL go M.empty
  where
    go :: Map String Int -> Synset Validated -> (Map String Int, DBSynset)
    go offsetMap synset =
      let synsetDB@DBSynset{synsetId = (lexFile, lexForm, lexId)} = synsetToDB relationsMap lexicographerMap index synset
      in ( M.insert (indexKey lexFile lexForm lexId) 0 offsetMap -- FIXME: add offset
         , synsetDB
         )

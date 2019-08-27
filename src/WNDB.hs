{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module WNDB where

import Data ( WNPOS(..), SynsetIdentifier(..), WordSenseIdentifier(..)
            , Synset(..), Validated, WNWord(..), WordSenseForm(..)
            , lexicographerFileIdToText, LexicographerFileId(..)
            , SynsetRelation(..), WordPointer(..) )
import Validate (Index, senseKey)       
---
import Data.List (findIndex)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.GenericTrie as T
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (Doc, pretty, (<+>))

data DBSynset = DBSynset
  { offset               :: Int
  , lexicographerFileNum :: Int
  , pos                  :: WNPOS
  , wordSenses           :: NonEmpty Text
  , gloss                :: Text
  , frames               :: [(Int,Int)]
  , relations            :: [(Text, SynsetIdentifier, WNPOS, (Int, Int))]
  } deriving (Show,Eq)

synsetToFauxDB :: Int -> Map Text Text -> Map Text Int -> Index (Synset a) -> Synset Validated -> DBSynset
synsetToFauxDB offset relationsMap lexicographerMap index
  Synset{lexicographerFileId, wordSenses, definition, examples, frames, relations} =
  DBSynset { offset = offset
           , lexicographerFileNum = unsafeLookup
                                    (lexicographerFileIdToText lexicographerFileId)
                                    lexicographerMap
           , pos = (\(LexicographerFileId (wnPos,_)) -> wnPos) lexicographerFileId
           , wordSenses = NE.map wordForm wordSenses -- FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition : examples) -- FIXME: add quotes
           , frames = map synsetFrame frames
                    ++ concatMap toWordFrames (zip [1..] $ NE.toList wordSenses)
           , relations = map synsetRelation relations
                         ++ concatMap wordRelations (zip [1..] $ NE.toList wordSenses)
           }
  where
    wordForm (WNWord (WordSenseIdentifier (_,WordSenseForm form,_)) _ _) = form
    synsetRelation (SynsetRelation relationName
                    wnIdentifier@(SynsetIdentifier (LexicographerFileId (wnPos,_),_,_)))
      = (unsafeLookup relationName relationsMap, wnIdentifier, wnPos, (0,0))
    synsetFrame = (0,)
    toWordFrames (ix, WNWord _ wordFrames _) = map (ix,) wordFrames
    wordRelations (ix, WNWord _ _ wordPointers) =
      map (wordRelation ix) wordPointers
    wordRelation ix (WordPointer pointerName
                       (WordSenseIdentifier wnIdentifier@(LexicographerFileId
                                                          (wnPos,_), _, _))) =
      ( unsafeLookup pointerName relationsMap
      , SynsetIdentifier wnIdentifier
      , wnPos
      , (ix, getWordSenseIndex wnIdentifier))
    unsafeLookup key map' = fromJust $ M.lookup key map'
    getSynsetWords (lexFileId,wordSenseForm,lexId) =
      let sensekey = senseKey lexFileId wordSenseForm lexId
      in case T.lookup sensekey index of
        Just Synset{wordSenses = synsetWords} -> NE.toList synsetWords
        Nothing -> error $ "No synset corresponding to sense key " ++ sensekey
    getWordSenseIndex wnIdentifier =
      let synsetWords = getSynsetWords wnIdentifier
      in case findIndex (\(WNWord (WordSenseIdentifier wnId) _ _) -> wnId == wnIdentifier) synsetWords of
        Just ix -> ix
        Nothing -> error $ "No wordsense corresponding to word sense " ++ show wnIdentifier

showSynsetDB :: DBSynset -> Doc a
showSynsetDB DBSynset{offset, lexicographerFileNum, pos, wordSenses}
  = offsetDoc
  <+> pretty lexicographerFileNum
  <+> synsetType pos
  <+> wordCount
  where
    wordCount = pretty $ NE.length wordSenses
    synsetType N = "n"
    synsetType V = "v"
    synsetType S = "s"
    synsetType A = "a"
    synsetType R = "r"
    offsetDoc = pretty . padText 8 . T.pack $ show offset
    padText n x = T.append (T.replicate (n - T.length x) "0") x

makeFauxDBs :: Map Text Text -> Map Text Int -> Index (Synset a) -> [Synset Validated]
  -> ([DBSynset], Map SynsetIdentifier Int)
makeFauxDBs relationsMap lexicographerMap index = foldl go ([], M.empty)
  where
    go (fauxDBsynsets, offsetMap) synset =
      ( synsetToFauxDB 0 relationsMap lexicographerMap index synset : fauxDBsynsets
      , offsetMap ) -- FIXME: add offset

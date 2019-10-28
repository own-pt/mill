{-# LANGUAGE FlexibleContexts #-}
module Validate
  ( validateSynsets
  , checkIndexNoDuplicates
  , DupsIndex
  , indexSynsets
  , lookupIndex
  , makeIndex
  , oneLangIndex
  , Index
  , indexKey
  , wordSenseKey
  ) where

import Data (Synset(..), Unvalidated, Validated, SourceValidation
            ,WNWord(..), WNid(..),SynsetId(..), Validation(..), WordSenseForm(..), LexicalId(..)
            ,SynsetRelation(..),WordPointer(..), WNValidation,WNError(..)
            ,WordSenseId(..),SourceError(..),singleton,toSourceError,lexicographerFileIdToText)

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Either (either,rights)
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.ListTrie.Base.Map (WrappedIntMap)
import Data.ListTrie.Patricia.Map (TrieMap,fromListWith',member
                                  ,toAscList,map',mapAccumWithKey',lookup)
import Prelude hiding (lookup)

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?
type Index a = TrieMap WrappedIntMap Char (Either String a)
type DupsIndex a = TrieMap WrappedIntMap Char (NonEmpty (Either String a))

makeIndex :: NonEmpty (Synset Unvalidated) -> DupsIndex (Synset Unvalidated)
makeIndex synsets = fromListWith' (<>) keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses = (headWordSense:|wordSenses)} =
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, singleton $ Right synset)
        : map ((, singleton $ Left headSenseKey) . wordSenseKey)
              wordSenses

checkIndexNoDuplicates :: DupsIndex (Synset Unvalidated)
  -> SourceValidation (Index (Synset Unvalidated))
checkIndexNoDuplicates index =
  case mapAccumWithKey' go [] index of
    ([], indexNoDuplicates) -> Success indexNoDuplicates
    (x:duplicateErrors, _)  -> Failure $ x:|duplicateErrors
  where
    go duplicateErrors _ (value :| [])
      = (duplicateErrors, value)
    go duplicateErrors key values
      = ( moreDuplicateErrors ++ duplicateErrors
        , NE.head values )
      where
        lookupKey headKey = case lookup headKey index of
          Just values' ->
            head . filter (elem key . NE.map wordSenseKey . wordSenses) . rights
            $ NE.toList values'
          Nothing -> error $ "Missing synset to key " ++ key
        moreDuplicateErrors
          = map (\value -> toSourceError (either lookupKey id value)
                . DuplicateWordSense $ takeWhile (/= '\t') key)
            $ NE.toList values

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord wnWordId _ _)
  = indexKey $ coerce wnWordId

indexKey :: WNid -> String
-- this is NOT the sensekey
indexKey WNid{wnName, pos, lexname, lexForm = WordSenseForm wordForm, lexId = LexicalId lexicalId} =
  -- if changing this definition change WNDB export too
  intercalate "\t" [ T.unpack wnName
                   , T.unpack wordForm
                   , show pos ++ T.unpack lexname
                   , pad $ show lexicalId
                   ]
  where
    pad x = replicate (2 - length x) '0' ++ x



---
-- checks
checkSynset :: Index a -> Synset Unvalidated -> SourceValidation (Synset Validated)
checkSynset index Synset{comments, lexicographerFileId, wordSenses, relations
                        , definition, examples, frames, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ NE.map (SourceError lexfileName sourcePosition) errors
  where
    lexfileName = lexicographerFileIdToText lexicographerFileId
    result = Synset
      <$> Success comments
      <*> Success sourcePosition
      <*> Success lexicographerFileId
      <*> checkWordSenses index wordSenses
      <*> Success definition
      <*> checkSortNoDuplicates UnsortedExamples DuplicateExamples examples
      <*> checkSortNoDuplicates UnsortedFrames DuplicateFrames frames
      <*> checkSynsetRelations index relations


--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

checkSynsetRelations :: Index a -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelations index synsetRelations
  =  checkSynsetRelationsOrderNoDuplicates
  *> checkSynsetRelationsTargets index synsetRelations
  *> Success synsetRelations
  where
    checkSynsetRelationsOrderNoDuplicates
      = checkSortNoDuplicates UnsortedSynsetRelations DuplicateSynsetRelation synsetRelations

checkSynsetRelationsTargets :: Index a -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ synsetId) =
      if member targetSenseKey index
      then Success synsetRelation
      else Failure (MissingSynsetRelationTarget synsetRelation :| [])
      where
        targetSenseKey = indexKey $ coerce synsetId

validateSorted :: Ord a => [a] -> Validation (NonEmpty (NonEmpty a)) [a]
-- maybe just sort input instead of picking some of the errors?
--- or maybe just check if every pair is sorted, which will be faster
--- but will take more iterations to find all errors
validateSorted (x:y:xt)
  | x <= y    = (:) <$> Success x <*> validateSorted (y:xt)
  | otherwise = let (wrongs, _) = span (< x) xt
                in (:) <$> Failure ((x:|(y:wrongs)):|[])
                       <*> validateSorted (y:xt)
validateSorted x = Success x

validateNoDuplicates :: Ord a => [a] -> Validation (NonEmpty a) [a]
validateNoDuplicates (x:y:xt)
  | x < y  = (:) <$> Success x <*> validateNoDuplicates (y:xt)
  | x == y = let (equals, rest) = span (== x) xt
             in (:) <$> Failure (x:|y:equals) <*> validateNoDuplicates rest
  | x > y  = error "Unsorted"
validateNoDuplicates x = Success x

checkSortNoDuplicates
  :: Ord a
  => (NonEmpty (NonEmpty a) -> WNError)
  -> (NonEmpty a -> WNError)
  -> [a]
  -> WNValidation [a]
checkSortNoDuplicates toSortError toDuplicateError = sortedCheckNoDuplicates . validateSorted
  where
    sortedCheckNoDuplicates (Failure unsortedSequences)
      = Failure (singleton $ toSortError unsortedSequences)
    sortedCheckNoDuplicates (Success xs)
      = bimap (singleton . toDuplicateError) id $ validateNoDuplicates xs

checkWordSenses :: Index a -> NonEmpty WNWord -> WNValidation (NonEmpty WNWord)
checkWordSenses index wordSenses
  =  checkWordSensesOrderNoDuplicates
  *> traverse (checkWordSense index) wordSenses
  *> Success wordSenses
  where
    checkWordSensesOrderNoDuplicates
      = checkSortNoDuplicates UnsortedWordSenses DuplicateSynsetWords . map wordSenseLexicalForm $ NE.toList wordSenses
    wordSenseLexicalForm (WNWord (WordSenseId WNid{lexForm = WordSenseForm lexicalForm}) _ _) = lexicalForm

checkWordSense :: Index a -> WNWord -> WNValidation WNWord
checkWordSense index wordSense@(WNWord _ _ wordPointers)
  =  checkWordSensePointersOrderNoDuplicates
  *> checkWordSensePointersTargets index wordPointers
  *> Success wordSense
  where
    checkWordSensePointersOrderNoDuplicates =
      checkSortNoDuplicates UnsortedWordPointers DuplicateWordRelation wordPointers
    

checkWordSensePointersTargets :: Index a -> [WordPointer]
  -> WNValidation [WordPointer]
checkWordSensePointersTargets index = traverse checkWordPointer
  where
    checkWordPointer wordPointer@(WordPointer _ wnWordId) =
      if member targetSenseKey index
      then Success wordPointer
      else Failure (MissingWordRelationTarget wordPointer :| []) -- []
      where
        targetSenseKey = indexKey $ coerce wnWordId

validateSynsets :: Index (Synset Unvalidated)
  -> NonEmpty (Synset Unvalidated)
  -> SourceValidation (NonEmpty (Synset Validated))
-- | validates synsets from the same lexicographer file against index
validateSynsets index (firstSynset:|synsets) =
  (:|)
  <$> checkSynset' firstSynset
  <*> foldr go (Success []) synsets
  where
    checkSynset' = checkSynset index
    go synset result = (:) <$> checkSynset' synset <*> result


indexSynsets :: Index (Synset Unvalidated) -> [Synset Unvalidated]
indexSynsets = concatMap (either (const []) (:[]) . snd) . toAscList

oneLangIndex :: Maybe Text -> Index (Synset Unvalidated) -> Index (Synset Unvalidated)
-- | Remove from the index all relations pointing to synsets in other
-- WNs; the index should not have any synsets from other WNs. in
-- normal operation this doesn't happen because these synsets do not
-- even get read when one language is specified
oneLangIndex Nothing = id
oneLangIndex (Just lang)
  = map' go
  where
    go (Left headKey) = Left headKey
    go (Right s@Synset{wordSenses,relations})
      = Right s{relations = filter oneLang relations, wordSenses = NE.map oneLangWordSense wordSenses}
      where
        oneLang (SynsetRelation _ (SynsetId WNid{wnName})) = wnName == lang
        oneLangWordSense (WNWord wID fs pointers)
          = WNWord wID fs
          $ filter oneLangPointer pointers
        oneLangPointer (WordPointer _ (WordSenseId WNid{wnName})) = wnName == lang

---

lookupIndex :: String -> Index (Synset a) -> Maybe (Synset a)
lookupIndex key index =
  case lookup key index of
    Just (Left headKey) -> lookupIndex headKey index
    Just (Right synset) -> Just synset
    Nothing -> Nothing

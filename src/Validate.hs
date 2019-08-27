module Validate
  ( validateSynsets
  , makeIndex
  ) where

import Data

import Data.Bifunctor (bimap)
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import Data.GenericTrie (Trie, fromListWith', member, lookup, foldWithKey, empty, insert)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Prelude hiding (lookup)
import Text.Megaparsec (PosState, SourcePos(..),unPos,attachSourcePos)

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?
type Index a = Trie String (Either String a) -- Left is a reference to another key

makeIndex :: NonEmpty (Synset Unvalidated) -> Trie String (NonEmpty (Either String (Synset Unvalidated)))
makeIndex synsets = fromListWith' (<>) keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses = (headWordSense:|wordSenses)} =
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, singleton $ Right synset) : map (\wordSense -> (wordSenseKey wordSense, singleton $ Left headSenseKey)) wordSenses

checkIndexNoDuplicates :: Trie String (NonEmpty (Either String (Synset Unvalidated)))
  -> Validation (NonEmpty (Int, WNError)) (Index (Synset Unvalidated))
checkIndexNoDuplicates index = foldWithKey go (Success empty) index
  where
    go key (value :| []) noDuplicatesTrie
      = Success (insert key value) <*> noDuplicatesTrie
    go key values noDuplicatesTrie
      = case map (\synset -> attachOffset synset . DuplicateWordSense $ takeWhile (/= '\t') key)
             $ duplicatesSynsets values of
          [] -> noDuplicatesTrie
          x:duplicateErrors -> Failure (x :| duplicateErrors) <*> noDuplicatesTrie
    duplicatesSynsets = mapMaybe synsetInfo . NE.toList
    synsetInfo (Right synset)
      = Just synset
    synsetInfo (Left reference)
      = case fromJust $ lookup reference index of
          (Right synset :| []) -> Just synset
          _ -> Nothing

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord (WordSenseIdentifier (lexicographerFileId, wordForm, lexicalId)) _ _)
  = senseKey lexicographerFileId wordForm lexicalId

senseKey :: LexicographerFileId -> WordSenseForm -> LexicalId -> String
-- [ ] this is not really a sense key
senseKey  (LexicographerFileId (pos, lexname)) (WordSenseForm wordForm) (LexicalId lexicalId) =
  intercalate "\t" [T.unpack wordForm, show pos ++ T.unpack lexname, show lexicalId]



---
-- checks
checkSynset :: Index a -> Synset Unvalidated -> Validation (NonEmpty (Int, WNError)) (Synset Validated)
checkSynset index unvalidatedSynset@Synset{lexicographerFileId, wordSenses, relations, definition
                        , examples, frames, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ NE.map (attachOffset unvalidatedSynset) errors
  where
    result = Synset
      <$> Success sourcePosition
      <*> Success lexicographerFileId
      <*> checkWordSenses index wordSenses
      <*> Success definition
      <*> Success examples
      <*> Success frames -- [ ] check frames
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
    checkSynsetRelation synsetRelation@(SynsetRelation _ (SynsetIdentifier (lexFileId, wordForm, lexicalId))) =
      if member targetSenseKey index
      then Success synsetRelation
      else Failure (MissingSynsetRelationTarget synsetRelation :| []) -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

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
    wordSenseLexicalForm (WNWord (WordSenseIdentifier (_,WordSenseForm lexicalForm,_)) _ _) = lexicalForm

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
    checkWordPointer wordPointer@(WordPointer _ (WordSenseIdentifier (lexFileId, wordForm, lexicalId))) =
      -- check pointer name too
      if member targetSenseKey index
      then Success wordPointer
      else Failure (MissingWordRelationTarget wordPointer :| []) -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

validateLexFileSynsets :: Trie String (NonEmpty (Either String (Synset Unvalidated)))
  -> (NonEmpty (Synset Unvalidated), PosState Text)
  -> SourceValidation (NonEmpty (Synset Validated))
validateLexFileSynsets indexWithDuplicates (firstSynset:|synsets, posState) =
  bimap liftErrors id . checkSynsetsOrder . checkSynsets $ checkIndexNoDuplicates indexWithDuplicates
  where
    liftErrors parseErrors
      = let (errors, _) = attachSourcePos fst parseErrors posState
        in NE.map toSourceError errors
    toSourceError ((_, wnError), SourcePos{sourceName, sourceLine, sourceColumn})
      = SourceError (T.pack sourceName)
          (SourcePosition (unPos sourceLine, unPos sourceColumn))
          wnError
    checkSynsetsOrder (Success validatedSynsets)
      = bimap (NE.map toError)
      NE.fromList . validateSorted $ NE.toList validatedSynsets
    checkSynsetsOrder (Failure es) = Failure es
    toError unsortedSynsetSequences@(synset:|_)
      = attachOffset synset $ UnsortedSynsets (unsortedSynsetSequences :| [])
    checkSynsets (Failure errors) = Failure errors
    checkSynsets (Success index)
      = (:|)
      <$> checkSynset' firstSynset
      <*> foldr go (Success []) synsets
      where
        checkSynset' = checkSynset index
        go synset result = (:) <$> checkSynset' synset <*> result

validateSynsets :: Trie String (NonEmpty (Either String (Synset Unvalidated)))
  -> NonEmpty (NonEmpty (Synset Unvalidated), PosState Text)
  -> SourceValidation (NonEmpty (Synset Validated))
validateSynsets indexWithDuplicates lexFileSynsetsPosStates =
  bimap id sconcat . sequenceA
  $ NE.map (validateLexFileSynsets indexWithDuplicates) lexFileSynsetsPosStates

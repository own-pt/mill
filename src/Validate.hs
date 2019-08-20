module Validate
  ( validateSynsets
  , makeIndex
  , Validation(..)
  , SourceValidation
  , SourceError(..)
  , WNError (..)
  ) where

import Data

import Data.Bifunctor (Bifunctor(..))
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie (Trie, fromListWith', member, lookup, foldWithKey, empty, insert)
import Data.Text.Prettyprint.Doc
  ( Pretty(..),Doc,(<+>), colon, align, hsep, nest
  , line, indent, vsep)
import Prelude hiding (lookup)

singleton :: a -> NonEmpty a
singleton x = x :| []

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

checkIndexNoDuplicates :: Trie String (NonEmpty (Either String (Synset Unvalidated))) -> SourceValidation (Index (Synset Unvalidated))
checkIndexNoDuplicates index = foldWithKey go (Success empty) index
  where
    go key (value :| []) noDuplicatesTrie
      = Success (insert key value) <*> noDuplicatesTrie
    go key values noDuplicatesTrie
      = case map (\synset -> toSourceError synset . DuplicateWordSense $ takeWhile (/= '\t') key)
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

---- validation
data Validation e a = Failure e | Success a deriving (Show,Eq)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Bifunctor Validation where
  bimap f _ (Failure e) = Failure (f e)
  bimap _ g (Success a) = Success (g a)

instance Semigroup e => Applicative (Validation e) where
  --  pure :: a -> Validation e a
  pure = Success
  --(<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  Success f <*> Success a  = Success (f a)
  Success _ <*> Failure e  = Failure e
  Failure e <*> Success _  = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

data WNError
  = ParseError String
  | DuplicateWordSense String
  | DuplicateSynsetWords (NonEmpty Text)
  | DuplicateWordRelation (NonEmpty WordPointer)
  | DuplicateSynsetRelation (NonEmpty SynsetRelation)
  | MissingSynsetRelationTarget SynsetRelation
  | MissingWordRelationTarget WordPointer
  | UnsortedSynsets (NonEmpty (NonEmpty (Synset Validated)))
  | UnsortedWordSenses (NonEmpty (NonEmpty Text))
  | UnsortedSynsetRelations  (NonEmpty (NonEmpty SynsetRelation))
  | UnsortedWordPointers (NonEmpty (NonEmpty WordPointer))
  deriving (Show)

data SourceError
  = SourceError Text -- ^ name of source file
                SourcePosition
                WNError deriving (Show)

toSourceError :: Synset a -> WNError -> SourceError
toSourceError Synset{sourcePosition, lexicographerFileId}
  = SourceError (lexicographerFileIdToText lexicographerFileId)
                sourcePosition

type WNValidation a = Validation (NonEmpty WNError) a
type SourceValidation a = Validation (NonEmpty SourceError) a

---
-- Pretty instances
prettyMissingTarget :: Text -> Text -> Doc ann -> Doc ann
prettyMissingTarget relationType relationName target
  =   "error: Missing"
  <+> pretty relationType
  <+> pretty relationName
  <+> "target" <+> target

prettyUnordered :: Pretty a => Text -> NonEmpty (NonEmpty a) -> Doc ann
prettyUnordered what sequences
  = "warning: Unsorted" <+> pretty what <> line
  <> (indent 2 . align . vsep . map prettyUnorderedSequence $ NE.toList sequences)
  where
    prettyUnorderedSequence (x:|xs) =
      pretty x <+> "should come after" <+> hsep (map pretty xs)

prettyDuplicate :: Pretty a => Text -> NonEmpty a -> Doc ann
prettyDuplicate what duplicates
  = "error: Duplicate"
  <+> pretty what
  <+> pretty (NE.head duplicates)
  
instance Pretty WNError where
  pretty (ParseError errorString) = pretty errorString
  pretty (DuplicateWordSense sensekey)
    = prettyDuplicate "wordsense" (singleton sensekey)
  pretty (DuplicateSynsetWords synsetWords)
    = prettyDuplicate "synset words" synsetWords
  pretty (DuplicateWordRelation wordPointers)
    = prettyDuplicate "word pointer" wordPointers
  pretty (DuplicateSynsetRelation synsetRelations)
    = prettyDuplicate "synset relation" synsetRelations
  pretty (MissingSynsetRelationTarget (SynsetRelation relationName target))
    = prettyMissingTarget "synset relation" relationName $ pretty target
  pretty (MissingWordRelationTarget (WordPointer pointerName target))
    = prettyMissingTarget "word pointer" pointerName $ pretty target
  pretty (UnsortedSynsets sequences)
    = prettyUnordered "synsets" sequences
  pretty (UnsortedSynsetRelations sequences)
    = prettyUnordered "synset relations" sequences
  pretty (UnsortedWordSenses sequences)
    = prettyUnordered "synset word senses" sequences
  pretty (UnsortedWordPointers sequences)
    = prettyUnordered "word pointers" sequences

instance Pretty SourceError where
  pretty (SourceError lexicographerFileId (SourcePosition (beg, end)) wnError)
    =   pretty lexicographerFileId
    <>  colon <> pretty beg <> colon <> pretty end <> colon
    <+> nest 2 (pretty wnError) <> line

---
-- checks
checkSynset :: Index a -> Synset Unvalidated -> SourceValidation (Synset Validated)
checkSynset index Synset{lexicographerFileId, wordSenses, relations, definition
                        , examples, frames, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ NE.map (SourceError lexfileName sourcePosition) errors
  where
    lexfileName = lexicographerFileIdToText lexicographerFileId
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

validateSynsets :: Trie String (NonEmpty (Either String (Synset Unvalidated)))
  -> NonEmpty (Synset Unvalidated)
  -> SourceValidation (NonEmpty (Synset Validated))
validateSynsets indexWithDuplicates (firstSynset:|synsets) =
  checkSynsetsOrder . checkSynsets $ checkIndexNoDuplicates indexWithDuplicates
  where
    checkSynsetsOrder (Success validatedSynsets)
      = bimap (NE.map toError) NE.fromList . validateSorted $ NE.toList validatedSynsets
    checkSynsetsOrder (Failure es) = Failure es
    toError unsortedSynsetSequences@(synset:|_)
      = toSourceError synset $ UnsortedSynsets (unsortedSynsetSequences :| [])
    checkSynsets (Failure errors) = Failure errors
    checkSynsets (Success index)
      = (:|)
      <$> checkSynset' firstSynset
      <*> foldr go (Success []) synsets
      where
        checkSynset' = checkSynset index
        go synset result = (:) <$> checkSynset' synset <*> result

---


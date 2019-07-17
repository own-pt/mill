module Validate where

import Data

import Data.List hiding (insert)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index a = Trie String (Either String a) -- Left is a reference to another key

makeIndex :: [Synset Unvalidated] -> Index (Synset Unvalidated)
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses = (headWordSense:|wordSenses)} =
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, Right synset) : map (\wordSense -> (wordSenseKey wordSense, Left headSenseKey)) wordSenses

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord (WordSenseIdentifier (lexicographerFileId, wordForm, lexicalId)) _ _) =
  senseKey lexicographerFileId wordForm lexicalId

senseKey :: LexicographerFileId -> WordSenseForm -> LexicalId -> String
-- [ ] this is not really a sense key
senseKey  (LexicographerFileId lexicographerFileId) (WordSenseForm wordForm) lexicalId =
  intercalate "\t" [T.unpack wordForm, T.unpack lexicographerFileId, show lexicalId]

---- validation
data Validation e a = Failure e | Success a deriving (Show,Eq)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validation e) where
  --  pure :: a -> Validation e a
  pure = Success
  --(<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  Success f <*> Success a  = Success (f a)
  Success _ <*> Failure e  = Failure e
  Failure e <*> Success _  = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

validation :: (e -> b) -> (a -> b) -> Validation e a -> b
validation f _ (Failure e) = f e
validation _ g (Success a) = g a

data WNError
  = MissingSynsetRelationTarget SynsetRelation
  | MissingWordRelationTarget WordPointer
  | UnsortedSynsetWordSenses (NonEmpty WordSenseForm)
  deriving (Show)

data SourceError = SourceError SourcePosition WNError deriving (Show)

showSourceError :: SourceError -> Text
-- use http://hackage.haskell.org/package/formatting-6.3.7/docs/Formatting.html ?
showSourceError (SourceError (SourcePosition pos) wnError) =
  T.concat ["[", T.pack $ show pos, "] ", showWNError wnError]
  where
    showWNError (MissingSynsetRelationTarget
                 (SynsetRelation relationName synsetId)) =
      T.concat ["Missing ", relationName, " relation target ", showSynsetId synsetId]
    showWNError (MissingWordRelationTarget
                 (WordPointer pointerName wordSenseId)) = T.concat ["Missing ", pointerName, " word relation target ", showWordSenseId wordSenseId]
    showWNError (UnsortedSynsetWordSenses sortedWordSenseForms) = T.concat ["Unsorted list of word senses; order should be ", showWordSenseForms sortedWordSenseForms]
    showWordSenseForms = T.intercalate ", " . NE.toList . NE.map (\(WordSenseForm wordSenseForm) -> wordSenseForm)
    showWordSenseId (WordSenseIdentifier wordSenseIdentifier) = showIdentifier wordSenseIdentifier
    showSynsetId (SynsetIdentifier synsetIdentifier) = showIdentifier synsetIdentifier
    showIdentifier (LexicographerFileId lexicographerFileId, WordSenseForm wordForm, LexicalId lexicalId) =
      T.concat [wordForm, ":", T.pack . show $ lexicalId, " at file ", lexicographerFileId]

checkSynset :: Index a -> Synset Unvalidated -> Validation [SourceError] (Synset Validated)
checkSynset index Synset{lexicographerFileId, wordSenses, relations, definition, examples, frames, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ map (SourceError sourcePosition) errors
  where
    result = Synset
      <$> Success sourcePosition
      <*> Success lexicographerFileId
      <*> checkWordSenses index wordSenses
      <*> Success definition
      <*> Success examples
      <*> Success frames -- [ ] check frames
      <*> checkSynsetRelationsTargets index relations


--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

-- [] also check order
checkSynsetRelationsTargets :: Index a -> [SynsetRelation]
  -> Validation [WNError] [SynsetRelation]
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ (SynsetIdentifier (lexFileId, wordForm, lexicalId))) =
      if member targetSenseKey index
      then Success synsetRelation
      else Failure [MissingSynsetRelationTarget synsetRelation] -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

checkWordSenses :: Index a -> NonEmpty WNWord -> Validation [WNError] (NonEmpty WNWord)
checkWordSenses index wordSenses =
  checkWordSensesOrder wordSenses <* checkWordSensesPointerTargets index wordSensePointers
  where
    wordSensePointers = concatMap (\(WNWord _ _ wordPointers) -> wordPointers) wordSenses


checkWordSensesPointerTargets :: Index a -> [WordPointer]
  -> Validation [WNError] [WordPointer]
checkWordSensesPointerTargets index = traverse checkWordPointer
  where
    checkWordPointer wordPointer@(WordPointer _ (WordSenseIdentifier (lexFileId, wordForm, lexicalId))) =
      -- check pointer name too
      if member targetSenseKey index
      then Success wordPointer
      else Failure [MissingWordRelationTarget wordPointer] -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

checkWordSensesOrder :: NonEmpty WNWord -> Validation [WNError] (NonEmpty WNWord)
checkWordSensesOrder wordSenses =
  if sortedWordForms /= wordForms
  then Failure [UnsortedSynsetWordSenses sortedWordForms]
  else Success wordSenses
  where
    sortedWordForms = NE.sort wordForms
    wordForms = NE.map (\(WNWord (WordSenseIdentifier (_, wordForm, _)) _ _) -> wordForm) wordSenses


--- https://www.reddit.com/r/haskell/comments/6zmfoy/the_state_of_logging_in_haskell/

validateIndex :: Index (Synset Unvalidated) -> Validation [SourceError] (Index (Synset Validated))
-- [ ] not validating if there are two things with the same reference
validateIndex index = foldWithKey go (Success empty) index
  where
    go key (Left headWordKey) result = insert key (Left headWordKey) <$> result
    go key (Right synset) result = insert key . Right <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index


validateSynsetsInIndex :: Index (Synset Unvalidated)
  -> Validation [SourceError] [Synset Validated]
validateSynsetsInIndex index = foldWithKey go (Success []) index
  where
    go _ (Left _) result = result
    go _ (Right synset) result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

validateSynsets :: Index (Synset Unvalidated) -> [Synset Unvalidated] -> Validation [SourceError] [Synset Validated]
validateSynsets index = foldr go (Success [])
  where
    go synset result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

---


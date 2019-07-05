{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Process where

--import Control.Monad
--import Data.Char
import Data.List hiding (insert)
import Data.List.NonEmpty(NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
--import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

import Parse hiding (synset,synsets,lexicalIdentifier,wordSensePointers)


data Synset = Synset
  { sourcePosition       :: SourcePosition
  , lexicographerFileId  :: LexicographerFileId
  , wordSenses           :: NonEmpty WNWord
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , relations            :: NonEmpty SynsetRelation
  } deriving (Show,Eq)


-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index a = Trie String (Either String a) -- Left is a reference to another key

makeIndex :: [SynsetToValidate] -> Index SynsetToValidate
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@SynsetToValidate{wordSenses = (headWordSense:|wordSenses)} = -- the fact that this is non-empty can be checked during parsing
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, Right synset) : map (\wordSense -> (wordSenseKey wordSense, Left headSenseKey)) wordSenses

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord (lexicographerFileId, wordForm, lexicalId) _ _) =
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

checkSynset :: Index a -> SynsetToValidate -> Validation [SourceError] Synset
checkSynset index SynsetToValidate{lexicographerFileId, wordSenses, relations, definition, examples, frames, sourcePosition} =
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
checkSynsetRelationsTargets :: Index a -> NonEmpty SynsetRelation
  -> Validation [WNError] (NonEmpty SynsetRelation)
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ (lexFileId, wordForm, lexicalId)) =
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
    checkWordPointer wordPointer@(WordPointer _ (lexFileId, wordForm, lexicalId)) =
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
    wordForms = NE.map (\(WNWord (_, wordForm, _) _ _) -> wordForm) wordSenses


--- https://www.reddit.com/r/haskell/comments/6zmfoy/the_state_of_logging_in_haskell/

validateSynsetsInIndex :: Index SynsetToValidate -> Validation [SourceError] (Index Synset)
-- [ ] not validating if there are two things with the same reference
validateSynsetsInIndex index = foldWithKey go (Success empty) index
  where
    go key (Left headWordKey) result = insert key (Left headWordKey) <$> result
    go key (Right synset) result = insert key . Right <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

validateSynsets :: Index SynsetToValidate -> [SynsetToValidate] -> Validation [SourceError] [Synset]
validateSynsets index = foldr go (Success [])
  where
    go synset result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

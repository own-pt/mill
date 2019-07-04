{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Process where

--import Control.Monad
--import Data.Char
import Data.List
--import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

import Parse hiding (synset,synsets,lexicalIdentifier,wordSensePointers)


data SynsetToValidate = SynsetToValidate
  { lexicographerFileId  :: Text
  , wordSenses           :: [WNWord]
  , relations            :: [SynsetRelation]
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , sourcePosition       :: Int
  } deriving (Show,Eq)

data Synset = Synset
  { lexicographerFileId  :: Text
  , wordSenses           :: [WNWord]
  , relations            :: [SynsetRelation]
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , sourcePosition       :: Int
  } deriving (Show,Eq)


processLexicographerFile :: (Text, Int, [(Int, [SynsetStatement])]) -> [SynsetToValidate]
processLexicographerFile (lexicographerFileName, _, synsetsComponents) =
  map go synsetsComponents
  where
    go (offset, synsetStatements) = toSynset (synsetSkeleton offset) synsetStatements
    synsetSkeleton = SynsetToValidate lexicographerFileName [] [] "" [] []
    toSynset :: SynsetToValidate -> [SynsetStatement] -> SynsetToValidate
    toSynset synset [] = synset
    toSynset synset@SynsetToValidate{wordSenses} (WordSenseStatement wnWord : remainingStatements) =
      toSynset (synset {wordSenses = wnWord : wordSenses}) remainingStatements
    toSynset synset@SynsetToValidate{relations} (SynsetRelationStatement relation : remainingStatements) =
      toSynset (synset {relations = relation : relations}) remainingStatements
    toSynset synset (DefinitionStatement definition : remainingStatements) =
      -- only one definition is allowed, and this has already been checked (TODO)
      toSynset (synset {definition = definition}) remainingStatements
    toSynset synset@SynsetToValidate{examples} (ExampleStatement example : remainingStatements) =
      toSynset (synset {examples = example : examples}) remainingStatements
    toSynset synset (FramesStatement frames : remainingStatements) =
      -- only one frames statement is allowed, and this has already been checked (TODO)
      toSynset (synset {frames = frames}) remainingStatements


-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index a = Trie String (Either String a) -- Left is a reference to another key

makeIndex :: [SynsetToValidate] -> Index SynsetToValidate
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@SynsetToValidate{wordSenses = (headWordSense:wordSenses)} = -- the fact that this is non-empty can be checked during parsing
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, Right synset) : map (\wordSense -> (wordSenseKey wordSense, Left headSenseKey)) wordSenses

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord (lexicographerFileId, wordForm, lexicalId) _ _) =
  senseKey lexicographerFileId wordForm lexicalId

senseKey :: Text -> Text -> Int -> String
-- [ ] this is not really a sense key
senseKey lexicographerFileId wordForm lexicalId =
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

data WNError = MissingSynsetRelationTarget SynsetRelation
  | MissingWordRelationTarget WordPointer
  | UnsortedSynsetWordSenses [Text]
  deriving (Eq,Show)
  -- [] how to include source info?

checkSynset :: Index a -> SynsetToValidate -> Validation [WNError] Synset
checkSynset index SynsetToValidate{lexicographerFileId, wordSenses, relations, definition, examples, frames, sourcePosition} =
  Synset <$>
  Success lexicographerFileId <*>
  checkWordSenses index wordSenses <*>
  checkSynsetRelationsTargets index relations <*>
  Success definition <*>
  Success examples <*>
  Success frames <*> -- [ ] check frames
  Success sourcePosition

--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

-- [] also check order
checkSynsetRelationsTargets :: Index a -> [SynsetRelation] -> Validation [WNError] [SynsetRelation]
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ (lexFileId, wordForm, lexicalId)) =
      if member targetSenseKey index
      then Success synsetRelation
      else Failure [MissingSynsetRelationTarget synsetRelation] -- []
      where
        targetSenseKey = senseKey wordForm lexFileId lexicalId

checkWordSenses :: Index a -> [WNWord] -> Validation [WNError] [WNWord]
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
        targetSenseKey = senseKey wordForm lexFileId lexicalId

checkWordSensesOrder :: [WNWord] -> Validation [WNError] [WNWord]
checkWordSensesOrder wordSenses =
  if sortedWordForms /= wordForms
  then Failure [UnsortedSynsetWordSenses sortedWordForms]
  else Success wordSenses
  where
    sortedWordForms = sort wordForms
    wordForms = map (\(WNWord (_, wordForm, _) _ _) -> wordForm) wordSenses


--- https://www.reddit.com/r/haskell/comments/6zmfoy/the_state_of_logging_in_haskell/

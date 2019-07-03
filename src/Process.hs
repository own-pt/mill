{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Process where

--import Control.Monad
--import Data.Char
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

import Parse hiding (synset,synsets,lexicalIdentifier)

data Synset = Synset
  { lexicographerFileId  :: Text
  , wordSenses           :: [WNWord]
  , relations            :: [SynsetRelation]
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , sourcePosition       :: Int
  } deriving (Show,Eq)


processLexicographerFile :: (Text, Int, [(Int, [SynsetStatement])]) -> [Synset]
processLexicographerFile (lexicographerFileName, _, synsetsComponents) =
  map go synsetsComponents
  where
    go (offset, synsetStatements) = toSynset (synsetSkeleton offset) synsetStatements
    synsetSkeleton = Synset lexicographerFileName [] [] "" [] []
    toSynset synset [] = synset
    toSynset synset@Synset{wordSenses} (WordSenseStatement wnWord : remainingStatements) =
      toSynset (synset {wordSenses = wnWord : wordSenses}) remainingStatements
    toSynset synset@Synset{relations} (SynsetRelationStatement relation : remainingStatements) =
      toSynset (synset {relations = relation : relations}) remainingStatements
    toSynset synset (DefinitionStatement definition : remainingStatements) =
      -- only one definition is allowed, and this has already been checked (TODO)
      toSynset (synset {definition = definition}) remainingStatements
    toSynset synset@Synset{examples} (ExampleStatement example : remainingStatements) =
      toSynset (synset {examples = example : examples}) remainingStatements
    toSynset synset (FramesStatement frames : remainingStatements) =
      -- only one frames statement is allowed, and this has already been checked (TODO)
      toSynset (synset {frames = frames}) remainingStatements

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index = Trie String (Either Text Synset)

makeIndex :: [Synset] -> Index
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{lexicographerFileId, wordSenses} =
      map (go synset lexicographerFileId) wordSenses
    go synset lexicographerFileId (WNWord (_, wordForm, lexicalIdentifier) _ _) =
      (senseKey wordForm lexicographerFileId lexicalIdentifier
      , Right synset) -- [ ] will using left in all other wordSenses save space?

senseKey :: Text --Int -> Int
  -> Text
  -> Int -> String
-- [ ] this is wrong
senseKey lemma lexicographerFileName lexicalIdentifier =
  intercalate ":" [T.unpack lemma, T.unpack lexicographerFileName, show lexicalIdentifier]      

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

checkSynset :: Index -> Synset -> Validation [WNError] Synset
checkSynset index synset =
  Success synset <* undefined

--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

checkSynsetRelation :: Index -> SynsetRelation -> Maybe WNError
checkSynsetRelation index synsetRelation@(SynsetRelation _ (maybeLexFileName, wordForm, lexicalId)) =
  if member wordSenseKey index
  then Nothing
  else Just $ MissingSynsetRelationTarget synsetRelation -- []
  where
    wordSenseKey = senseKey wordForm (fromJust maybeLexFileName) lexicalId

checkWordPointer :: Index -> WordPointer -> Maybe WNError
checkWordPointer index wordPointer@(WordRelation _ (maybeLexFileName, wordForm, lexicalId)) =
  -- check pointer name too
  if member wordSenseKey index
  then Nothing
  else Just $ MissingWordRelationTarget wordPointer -- []
  where
    wordSenseKey = senseKey wordForm (fromJust maybeLexFileName) lexicalId

checkWordSensesOrder :: [WNWord] -> Maybe WNError
checkWordSensesOrder wordSenses =
  if sortedWordForms /= wordForms
  then Just $ UnsortedSynsetWordSenses sortedWordForms
  else Nothing
  where
    sortedWordForms = sort wordForms
    wordForms = map (\(WNWord (_, wordForm,_) _ _) -> wordForm) wordSenses

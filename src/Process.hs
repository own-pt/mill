{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Process where

--import Control.Monad
--import Data.Char
import Data.Maybe
import Data.List
--import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

import Parse hiding (synset,synsets,lexicalIdentifier,wordSensePointers)

type WordSenseIdentifier = ( Text -- ^ Lexicographer file identifier
                           , Text -- ^ Word form
                           , Int  -- ^ Lexical identifier
                           )

type SynsetIdentifier = WordSenseIdentifier

data WordPointer = WordPointer PointerName WordSenseIdentifier
  deriving (Show,Eq)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Show,Eq)
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Show,Eq)

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
      toSynset (synset {wordSenses = explicitWNWord lexicographerFileName wnWord : wordSenses}) remainingStatements
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

explicitWordSenseIdentifier :: Text -> ImplicitWordSenseIdentifier -> WordSenseIdentifier
explicitWordSenseIdentifier defaultLexicographerId (maybeLexicographerId, wordForm, lexicalId) =
  (fromMaybe defaultLexicographerId maybeLexicographerId, wordForm, lexicalId)

explicitWNWord :: Text -> ImplicitWNWord -> WNWord
explicitWNWord defaultLexicographerId (ImplicitWNWord implicitWordSenseIdentifier frameIds wordPointers) =
  WNWord (explicitWordSenseId implicitWordSenseIdentifier)
    frameIds $ map explicitWordPointer wordPointers
  where
    explicitWordSenseId = explicitWordSenseIdentifier defaultLexicographerId
    explicitWordPointer (ImplicitWordPointer pointerName implicitWordSenseIdentifier) =
      WordPointer pointerName
        $ explicitWordSenseId implicitWordSenseIdentifier

explicitSynsetRelation :: Text -> 

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index = Trie String (Either Text SynsetToValidate)

makeIndex :: [SynsetToValidate] -> Index
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@SynsetToValidate{lexicographerFileId, wordSenses} =
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

checkSynset :: Index -> SynsetToValidate -> Validation [WNError] Synset
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
checkSynsetRelationsTargets :: Index -> [SynsetRelation] -> Validation [WNError] [SynsetRelation]
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ (maybeLexFileName, wordForm, lexicalId)) =
      if member wordSenseKey index
      then Success synsetRelation
      else Failure [MissingSynsetRelationTarget synsetRelation] -- []
      where
        wordSenseKey = senseKey wordForm (fromJust maybeLexFileName) lexicalId

checkWordSenses :: Index -> [WNWord] -> Validation [WNError] [WNWord]
checkWordSenses index wordSenses =
  checkWordSensesOrder wordSenses <* checkWordSensesPointerTargets index wordSensePointers
  where
    wordSensePointers = concatMap (\(WNWord _ _ wordPointers) -> wordPointers) wordSenses 


checkWordSensesPointerTargets :: Index -> [WordPointer]
  -> Validation [WNError] [WordPointer]
checkWordSensesPointerTargets index = traverse checkWordPointer
  where
    checkWordPointer wordPointer@(WordPointer _ (maybeLexFileName, wordForm, lexicalId)) =
      -- check pointer name too
      if member wordSenseKey index
      then Success wordPointer
      else Failure [MissingWordRelationTarget wordPointer] -- []
      where
        wordSenseKey = senseKey wordForm (fromJust maybeLexFileName) lexicalId

checkWordSensesOrder :: [WNWord] -> Validation [WNError] [WNWord]
checkWordSensesOrder wordSenses =
  if sortedWordForms /= wordForms
  then Failure [UnsortedSynsetWordSenses sortedWordForms]
  else Success wordSenses
  where
    sortedWordForms = sort wordForms
    wordForms = map (\(WNWord (_, wordForm,_) _ _) -> wordForm) wordSenses


--- https://www.reddit.com/r/haskell/comments/6zmfoy/the_state_of_logging_in_haskell/

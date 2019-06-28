{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module ProcessParse where

import Control.Monad
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Parse hiding (synset)

data Synset = Synset
  { lexicographerFileId  :: Int
  , wordSenses           :: [WNWord]
  , relations            :: [SynsetRelation]
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , sourcePosition       :: Int
  } deriving (Show,Eq)

processLexicographerFile :: Int -> [[SynsetStatement]] -> [Synset]
-- RawSynsetStatement will become SynsetStatements after error reporting
processLexicographerFile lexicographerFileId =
  map (toSynset synsetSkeleton)
  where
    synsetSkeleton = Synset lexicographerFileId [] [] "" [] [] 0
    toSynset synset [] = synset
    toSynset synset@Synset{wordSenses} (WordSenseStatement wnWord : remainingStatements) =
      toSynset (synset {wordSenses = wnWord : wordSenses}) remainingStatements
    toSynset synset@Synset{relations} (SynsetRelationStatement relation : remainingStatements) =
      toSynset (synset {relations = relation : relations}) remainingStatements
    toSynset synset (DefinitionStatement definition : remainingStatements) =
      -- only one definition is allowed, and this has already been checked
      toSynset (synset {definition = definition}) remainingStatements
    toSynset synset@Synset{examples} (ExampleStatement example : remainingStatements) =
      toSynset (synset {examples = example : examples}) remainingStatements
    toSynset synset (FramesStatement frames : remainingStatements) =
      -- only one frames statement is allowed, and this has already been checked
      toSynset (synset {frames = frames}) remainingStatements

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

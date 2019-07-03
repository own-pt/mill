{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Process where

--import Control.Monad
--import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie

import Parse hiding (synset,synsets,lexicalIdentifier)

data Synset = Synset
  { lexicographerFileId  :: Int
  , wordSenses           :: [WNWord]
  , relations            :: [SynsetRelation]
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , sourcePosition       :: Int
  } deriving (Show,Eq)

processLexicographerFile :: (Text, Int, [(Int, [SynsetStatement])]) -> [Synset]
-- RawSynsetStatement will become SynsetStatements after error reporting
processLexicographerFile (_, lexicographerFileId, synsetsComponents) =
  map go synsetsComponents
  where
    go (offset, synsetStatements) = toSynset (synsetSkeleton offset) synsetStatements
    synsetSkeleton = Synset lexicographerFileId [] [] "" [] []
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

type Index = Trie String (Either Text Synset)

makeIndex :: [Synset] -> Index
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{lexicographerFileId, wordSenses} =
      map (go synset lexicographerFileId) wordSenses
    go synset lexicographerFileId (WNWord (_, wordForm, lexicalIdentifier) _ _) =
      (senseKey wordForm 1 -- [ ]
       lexicographerFileId lexicalIdentifier, Right synset) -- [ ] will using left in all other wordSenses save space?

senseKey :: Text -> Int -> Int -> Int -> String
-- [ ] this is wrong
senseKey lemma synsetType lexicographerFileId lexicalIdentifier =
  intercalate ":" [T.unpack lemma ++ "%", show synsetType, show lexicographerFileId, show lexicalIdentifier]

checkSynsetRelation :: Index -> SynsetRelation -> Either String SynsetRelation
checkSynsetRelation = _

checkWordPointer :: Index -> WordPointer -> Either String WordPointer
checkWordPointer = _

checkWordSenses :: [WNWord] -> Either String [WNWord]
checkWordSenses = _

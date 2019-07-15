{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty(NonEmpty)
import Data.RDF.Types (Subject(..), Predicate(..), Object(..),
                       IRI(..), Triple(..))
import Data.RDF.ToRDF (ToRDF(..),ToObject(..), RDFGen, runRDFGen, appBaseIRI,Triples)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Reader (ask)
import Data.Monoid (Endo(..))
import qualified Data.DList as DL


newtype LexicographerFileId = LexicographerFileId Text deriving (Show,Eq,Ord,ToObject)

newtype WordSenseForm = WordSenseForm Text deriving (Show,Eq,Ord, ToObject)

newtype LexicalId = LexicalId Int deriving (Show,Eq,Ord,ToObject)

newtype WordSenseIdentifier =
  WordSenseIdentifier ( LexicographerFileId
                      , WordSenseForm
                      , LexicalId
                      ) deriving (Show,Eq,Ord)

makeWordSenseIdentifier :: LexicographerFileId -> WordSenseForm -> LexicalId
  -> WordSenseIdentifier
makeWordSenseIdentifier lexicographerId wordSenseForm lexicalId =
  WordSenseIdentifier (lexicographerId, wordSenseForm, lexicalId)

newtype SynsetIdentifier =
  SynsetIdentifier ( LexicographerFileId
                   , WordSenseForm
                   , LexicalId
                   ) deriving (Show,Eq,Ord)

type PointerName = Text
type RelationName = Text
data WordPointer = WordPointer PointerName WordSenseIdentifier
  deriving (Show,Eq)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Show,Eq)
type FrameIdentifier = Int
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Show,Eq)

newtype SourcePosition = SourcePosition (Int, Int) deriving (Show,Eq,Ord)

-- synsets can be 
data Unvalidated
data Validated

data Synset a = Synset
  { sourcePosition       :: SourcePosition
  , lexicographerFileId  :: LexicographerFileId
  , wordSenses           :: NonEmpty WNWord
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , relations            :: NonEmpty SynsetRelation
  } deriving (Show,Eq)

---
-- to RDF instances

wnIdentifierToIRI :: Text -> (LexicographerFileId, WordSenseForm, LexicalId) -> RDFGen IRI
wnIdentifierToIRI prefix (LexicographerFileId lexicoId, WordSenseForm wForm, LexicalId lId) =
  go <$> ask
  where
    go baseIri = baseIri {iriPath = T.concat [prefix, "-", lexicoId, "-", wForm, "-", T.pack $ show lId]}

wordSenseIdentifierToIRI :: WordSenseIdentifier -> RDFGen IRI
wordSenseIdentifierToIRI (WordSenseIdentifier wnIdentifier) = wnIdentifierToIRI "wordsense" wnIdentifier

synsetIdentifierToIRI :: SynsetIdentifier -> RDFGen IRI
synsetIdentifierToIRI (SynsetIdentifier wnIdentifier) = wnIdentifierToIRI "synset" wnIdentifier



synsetToTriples :: Synset Validated -> RDFGen Triples
synsetToTriples Synset{lexicographerFileId, wordSenses, definition} = do
  lexicographerFileLiteral <- object lexicographerFileId
  synsetIri <- synsetIriGen
  lexicographerFilePredicate <- appBaseIRI $ Endo (\baseIri -> baseIri {iriPath = "inLexicographerFile"})
  definitionPredicate <- appBaseIRI $ Endo (\baseIri -> baseIri {iriPath = "definition"})
  definitionLiteral <- object definition
  return $ DL.fromList
    [ Triple (IRISubject synsetIri) (Predicate lexicographerFilePredicate) lexicographerFileLiteral
    , Triple (IRISubject synsetIri) (Predicate definitionPredicate) definitionLiteral]
  where
    synsetIriGen = synsetIdentifierToIRI (SynsetIdentifier headWordId)
    headWordId = (\(WNWord (WordSenseIdentifier wnIdentifier) _ _) -> wnIdentifier) $ NE.head wordSenses

-- instance ToRDF (Synset a) where
--   triples = pure . _



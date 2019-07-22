{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty(NonEmpty)
import Data.RDF.Types (Subject(..), Predicate(..), Object(..),
                       IRI(..), Triple(..),Literal(..),LiteralType(..))
import Data.RDF.ToRDF (ToRDF(..),ToObject(..), RDFGen, appBaseIRI,Triples)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Trans.Reader (ask)
import Data.Monoid (Endo(..))
import qualified Data.DList as DL


data WNPOS = N | V | R | A | S deriving (Show,Eq,Enum,Ord)
newtype LexicographerFileId = LexicographerFileId (WNPOS, Text) deriving (Show,Eq,Ord)

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
  , relations            :: [SynsetRelation] -- [] use NonEmpty if not for a relationless adjectives?
  } deriving (Show,Eq)

---
-- to RDF instances

lexicographerFileIdToText :: LexicographerFileId -> Text
lexicographerFileIdToText (LexicographerFileId (wnPOS, filename)) = T.append (posText wnPOS) filename
  where
    posText N = "noun"
    posText V = "verb"
    posText A = "adj"
    posText S = "adjs"
    posText R = "adv"

lexicographerFileIdFromText :: Text -> Maybe LexicographerFileId
lexicographerFileIdFromText = go . T.breakOn "."
  where
    wrap pos name = Just $ LexicographerFileId (pos, name)
    go ("noun",name) = wrap N name
    go ("verb",name) = wrap V name
    go ("adj",name)  = wrap A name
    go ("adjs",name) = wrap S name
    go ("adv",name)  = wrap R name
    go _             = Nothing
    
instance ToObject LexicographerFileId where
  object lexicographerFileId = pure . LiteralObject $ Literal (lexicographerFileIdToText lexicographerFileId) LiteralUntyped

wnIdentifierToIRI :: Text -> (LexicographerFileId, WordSenseForm, LexicalId) -> RDFGen IRI
wnIdentifierToIRI prefix (lexicographerFileId, WordSenseForm wForm, LexicalId lId) =
  go <$> ask
  where
    go baseIri = baseIri {iriPath = T.concat [prefix, "-", lexicographerFileIdToText lexicographerFileId, "-", wForm, "-", T.pack $ show lId]}

wordSenseIdentifierToIRI :: WordSenseIdentifier -> RDFGen IRI
wordSenseIdentifierToIRI (WordSenseIdentifier wnIdentifier) = wnIdentifierToIRI "wordsense" wnIdentifier

synsetIdentifierToIRI :: SynsetIdentifier -> RDFGen IRI
synsetIdentifierToIRI (SynsetIdentifier wnIdentifier) = wnIdentifierToIRI "synset" wnIdentifier

synsetToTriples :: Synset Validated -> RDFGen Triples
synsetToTriples Synset{lexicographerFileId, wordSenses, definition, examples, frames} = do
  lexicographerFileLiteral <- object lexicographerFileId
  synsetIri <- fmap IRISubject synsetIriGen
  lexicographerFilePredicate <- makePredicate "lexicographerFile"
  definitionPredicate <- makePredicate "definition"
  definitionLiteral <- object definition
  examplePredicate <- makePredicate "example"
  exampleLiterals <- mapM object examples
  containsWordSensePredicate <- makePredicate "containsWordSense"
  wordSenseObjs <- fmap (map IRIObject) . mapM wordSenseIri $ NE.toList wordSenses
  framePredicate <- makePredicate "frame"
  frameLiterals <- mapM object frames
  return . DL.concat . map DL.fromList $ [
    [ Triple synsetIri lexicographerFilePredicate lexicographerFileLiteral
    , Triple synsetIri definitionPredicate definitionLiteral ]
    , map (Triple synsetIri examplePredicate) exampleLiterals
    , map (Triple synsetIri containsWordSensePredicate) wordSenseObjs
    , map (Triple synsetIri framePredicate) frameLiterals ]
  where
    wordSenseIri (WNWord wordSenseId _ _) = wordSenseIdentifierToIRI wordSenseId
    makePredicate path = fmap Predicate . appBaseIRI $ Endo (\baseIri -> baseIri {iriPath = path})
    synsetIriGen = synsetIdentifierToIRI (SynsetIdentifier headWordId)
    headWordId = (\(WNWord (WordSenseIdentifier wnIdentifier) _ _) -> wnIdentifier) $ NE.head wordSenses

instance ToRDF (Synset Validated) where
   triples = synsetToTriples



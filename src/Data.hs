{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Data where

import Control.Monad.Trans.Reader (ask)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.DList as DL
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.RDF.ToRDF (ToObject(..), RDFGen, appBaseIRI,Triples)
import Data.RDF.Types (Subject(..), Predicate(..), Object(..),
                       IRI(..), Triple(..),Literal(..),LiteralType(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ( Pretty(..),Doc,dot,colon,(<+>), nest
                                 , line, indent, align, vsep, hsep)


singleton :: a -> NonEmpty a
singleton x = x :| []

data WNPOS = A | S | R | N | V deriving (Show,Eq,Enum,Ord)

newtype LexicographerFileId = LexicographerFileId (WNPOS, Text) deriving (Show,Eq,Ord)

newtype WordSenseForm = WordSenseForm Text deriving (Show,Eq,Ord,Pretty,ToObject)

newtype LexicalId = LexicalId Int deriving (Show,Eq,Ord,Pretty,ToObject)

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
  deriving (Show,Eq,Ord)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Show,Eq,Ord)
type FrameIdentifier = Int
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Show,Eq,Ord)

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

instance Ord (Synset Validated) where
  Synset{wordSenses = (headWord:|_)} <= Synset{wordSenses = (headWord2:|_)}
    = headWord <= headWord2

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

--- Pretty instances
instance Pretty WNPOS where
  pretty N = "noun"
  pretty V = "verb"
  pretty R = "adv"
  pretty A = "adj"
  pretty S = "adj"

instance Pretty LexicographerFileId where
  pretty (LexicographerFileId (wnPOS, lexicographerName)) =
    pretty wnPOS <> dot <> pretty lexicographerName

prettyIdentifier :: (LexicographerFileId, WordSenseForm, LexicalId) -> Doc ann
prettyIdentifier (lexicographerFileId, wordSenseForm, lexicalId)
  =   pretty lexicographerFileId
  <>  colon
  <>  pretty wordSenseForm
  <+> pretty lexicalId

instance Pretty WordSenseIdentifier where
  pretty (WordSenseIdentifier wnIdentifier) = prettyIdentifier wnIdentifier

instance Pretty SynsetIdentifier where
  pretty (SynsetIdentifier wnIdentifier) = prettyIdentifier wnIdentifier

prettyRelation :: Text -> (LexicographerFileId, WordSenseForm, LexicalId) -> Doc ann
prettyRelation name wnIdentifier = pretty name <> "»" <> prettyIdentifier wnIdentifier

instance Pretty WordPointer where
  pretty (WordPointer pointerName (WordSenseIdentifier wnIdentifier))
    = prettyRelation pointerName wnIdentifier

instance Pretty SynsetRelation where
  pretty (SynsetRelation relationName (SynsetIdentifier wnIdentifier))
    = prettyRelation relationName wnIdentifier

instance Pretty WNWord where
  pretty (WNWord (WordSenseIdentifier wnIdentifier) _ _)
    = prettyIdentifier wnIdentifier

instance Pretty (Synset a) where
  pretty Synset{wordSenses = wordSense:|_} = pretty wordSense

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
-- to RDF instances

lexicographerFileIdToText :: LexicographerFileId -> Text
lexicographerFileIdToText (LexicographerFileId (wnPOS, filename)) =
  T.concat [posText wnPOS, ".", filename]
  where
    posText N = "noun"
    posText V = "verb"
    posText A = "adj"
    posText S = "adjs"
    posText R = "adv"

lexicographerFileIdFromText :: Text -> Maybe LexicographerFileId
lexicographerFileIdFromText = go . T.breakOn "."
  where
    wrap pos name = Just $ LexicographerFileId (pos, T.tail name)
    go ("noun",name) = wrap N name
    go ("verb",name) = wrap V name
    go ("adj",name)  = wrap A name
    go ("adjs",name) = wrap S name
    go ("adv",name)  = wrap R name
    go _             = Nothing

instance ToObject LexicographerFileId where
  object lexicographerFileId
    = pure . LiteralObject
    $ Literal (lexicographerFileIdToText lexicographerFileId) LiteralUntyped

wnIdentifierToIRI :: Text -> (LexicographerFileId, WordSenseForm, LexicalId) -> RDFGen IRI
wnIdentifierToIRI prefix (lexicographerFileId, WordSenseForm wForm, LexicalId lId) =
  go <$> ask
  where
    go baseIri@IRI{iriPath}
      = baseIri { iriPath = T.concat [ iriPath
                                     , prefix, "-"
                                     , lexicographerFileIdToText lexicographerFileId
                                     , "-", wForm, "-", T.pack $ show lId
                                     ]
                }

wordSenseIdIRI :: WordSenseIdentifier -> RDFGen IRI
wordSenseIdIRI (WordSenseIdentifier wnIdentifier) =
  wnIdentifierToIRI "wordsense" wnIdentifier

synsetIdentifierToIRI :: SynsetIdentifier -> RDFGen IRI
synsetIdentifierToIRI (SynsetIdentifier wnIdentifier) = wnIdentifierToIRI "synset" wnIdentifier

synsetToTriples :: Map Text Text -> Synset Validated -> RDFGen Triples
synsetToTriples relationsMap Synset{lexicographerFileId, wordSenses, definition, examples
                                   , frames = synsetFrames, relations} = do
  lexicographerFileLiteral   <- object lexicographerFileId
  synsetIri                  <- IRISubject <$> synsetIriGen
  lexicographerFilePredicate <- makePredicate "lexicographerFile"
  definitionPredicate        <- makePredicate "definition"
  definitionLiteral          <- object definition
  examplePredicate           <- makePredicate "example"
  exampleLiterals            <- mapM object examples
  containsWordSensePredicate <- makePredicate "containsWordSense"
  wordSenseObjs              <- map IRIObject <$>
                                  mapM wordSenseIRI wordSenses'
  framePredicate             <- makePredicate "frame"
  frameLiterals              <- mapM object synsetFrames
  wordSenseTriples           <- concat <$> mapM wordSenseToTriples wordSenses'
  relationPredicates         <- mapM (makePredicate
                                      . (\(SynsetRelation relationName _) -> relationName))
                                     relations
  targetSynsetObjs           <- map IRIObject
                                  <$> mapM (synsetIdentifierToIRI
                                            . (\(SynsetRelation _ targetSynsetId) -> targetSynsetId))
                                           relations
  return . DL.concat $ map DL.fromList [
    [ Triple synsetIri lexicographerFilePredicate lexicographerFileLiteral
    , Triple synsetIri definitionPredicate definitionLiteral ]
    , map (Triple synsetIri examplePredicate) exampleLiterals
    , map (Triple synsetIri containsWordSensePredicate) wordSenseObjs
    , map (Triple synsetIri framePredicate) frameLiterals
    , wordSenseTriples
    , zipWith (Triple synsetIri) relationPredicates targetSynsetObjs
    ]
  where
    wordSenseIRI (WNWord wordSenseId _ _) = wordSenseIdIRI wordSenseId
    wordSenses' = NE.toList wordSenses
    makePredicate relationName
      = fmap Predicate . appBaseIRI
      $ Endo (\baseIri -> baseIri {iriPath = toRDFName relationName})
    toRDFName relationName =
      fromMaybe (error $ "Inexisting relation " ++ T.unpack relationName ++ " on relations.tsv")
            $ M.lookup relationName relationsMap
    synsetIriGen = synsetIdentifierToIRI (SynsetIdentifier headWordId)
    headWordId = (\(WNWord (WordSenseIdentifier wnIdentifier) _ _) -> wnIdentifier)
      $ NE.head wordSenses
    wordSenseToTriples :: WNWord -> RDFGen [Triple]
    wordSenseToTriples wordSense@(WNWord (WordSenseIdentifier (_, wordForm, _)) frames pointers) = do
      wordSenseIri         <- IRISubject <$> wordSenseIRI wordSense
      lexicalFormPredicate <- makePredicate "lexicalForm"
      lexicalForm          <- object wordForm
      framePredicate       <- makePredicate "frame"
      frameLiterals        <- mapM object frames
      pointersPredicates   <- mapM (makePredicate . (\(WordPointer pointerName _) -> pointerName))
                                   pointers
      targetWordSenseObjs  <- map IRIObject
                                <$> mapM (wordSenseIdIRI
                                          . (\(WordPointer _ targetWordSenseId) -> targetWordSenseId))
                                   pointers
      return $ concat [
        [
          Triple wordSenseIri lexicalFormPredicate lexicalForm
        ]
        , map (Triple wordSenseIri framePredicate) frameLiterals
        , zipWith (Triple wordSenseIri) pointersPredicates targetWordSenseObjs
        ]

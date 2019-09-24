{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data where

import Data.Aeson ( ToJSON(..), Value(..) )
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (Binary)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ( Pretty(..),Doc,dot,colon,(<+>), nest
                                 , line, indent, align, vsep, hsep)
import GHC.Generics (Generic)
import Text.Printf (printf)

singleton :: a -> NonEmpty a
singleton x = x :| []

data WNObj = SynsetObj | WordObj deriving (Binary,Eq,Enum,Generic)

instance Show WNObj where
  show SynsetObj = "synset"
  show WordObj   = "word"

readWNObj :: Text -> WNObj
readWNObj input = case input of
  "synset" -> SynsetObj
  "word"   -> WordObj
  _        -> error . T.unpack
    $ T.intercalate " " ["Can't parse", input, "as WordNet object name (one of synset or word)"]

unsafeLookup :: Ord k => String -> k -> Map k a -> a
unsafeLookup errorMessage = M.findWithDefault $ error errorMessage

data WNPOS = A | S | R | N | V deriving (Binary,Eq,Enum,Generic,Ord,Show,ToJSON)

readShortWNPOS :: Text -> WNPOS
readShortWNPOS "n" = N
readShortWNPOS "a" = A
readShortWNPOS "r" = R
readShortWNPOS "v" = V
readShortWNPOS "s" = S
readShortWNPOS input = error $ T.unpack input ++ " is not a valid PoS"

readLongWNPOS :: Text -> Maybe WNPOS
readLongWNPOS "noun" = Just N
readLongWNPOS "verb" = Just V
readLongWNPOS "adjs" = Just S
readLongWNPOS "adj"  = Just A
readLongWNPOS "adv"  = Just R
readLongWNPOS _      = Nothing

newtype LexicographerFileId = LexicographerFileId (WNPOS, Text)
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)

synsetType :: WNPOS -> Int
synsetType N = 1
synsetType V = 2
synsetType A = 3
synsetType R = 4
synsetType S = 5

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
    wrap pos name = LexicographerFileId (pos
                                        , T.tail name) -- remove '.'
    go (pos, name) = wrap <$> readLongWNPOS pos <*> Just name

newtype WordSenseForm = WordSenseForm Text
  deriving (Eq,Ord,Generic,Show)
  deriving newtype (Binary,Pretty,ToJSON)

newtype LexicalId = LexicalId Int
  deriving (Eq,Generic,Ord,Show)
  deriving newtype (Binary,Pretty,ToJSON)

tshow :: Show a => a -> Text
tshow = T.pack . show

wnIdentifierToJSON :: ( LexicographerFileId, WordSenseForm , LexicalId) -> Value
wnIdentifierToJSON (LexicographerFileId (wnPOS, lexname), WordSenseForm wordSenseForm, LexicalId lexId)
  = toJSON (wnPOS, lexname, wordSenseForm, lexId)

newtype WordSenseIdentifier =
  WordSenseIdentifier ( LexicographerFileId
                      , WordSenseForm
                      , LexicalId
                      )
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary)

instance ToJSON WordSenseIdentifier where
  toJSON (WordSenseIdentifier wnIdentifier) = wnIdentifierToJSON wnIdentifier

makeWordSenseIdentifier :: LexicographerFileId -> WordSenseForm -> LexicalId
  -> WordSenseIdentifier
makeWordSenseIdentifier lexicographerId wordSenseForm lexicalId =
  WordSenseIdentifier (lexicographerId, wordSenseForm, lexicalId)

newtype SynsetIdentifier =
  SynsetIdentifier ( LexicographerFileId
                   , WordSenseForm
                   , LexicalId
                   )
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary)

instance ToJSON SynsetIdentifier where
  toJSON (SynsetIdentifier wnIdentifier) = wnIdentifierToJSON wnIdentifier

type PointerName = Text
type RelationName = Text
data WordPointer = WordPointer PointerName WordSenseIdentifier
  deriving (Binary,Eq,Generic,Ord,Show,ToJSON)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Binary,Eq,Generic,Ord,Show,ToJSON)
type FrameIdentifier = Int
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Binary,Eq,Generic,Ord,Show)

senseKey :: Int -> Int -> Maybe SynsetRelation -> WNWord -> String
senseKey lexFileNum synsetTypeNum maybeHeadRelation
  wordSense@(WNWord (WordSenseIdentifier (_,WordSenseForm wordForm,LexicalId lexicalId)) _ _)
  = printf "%s%%%d:%02d:%02d:%s:%s" lemma synsetTypeNum lexFileNum
                                lexicalId headWordForm (headWordLexicalId :: String)
  where
    lemma = T.toLower wordForm
    (headWordForm, headWordLexicalId) =
      case (synsetTypeNum, maybeHeadRelation) of
        (5, Just (SynsetRelation _
                  (SynsetIdentifier (_,WordSenseForm headForm,LexicalId headLexicalId))))
          -> (T.toLower headForm, printf "%02d" headLexicalId)
        (5,Nothing) -> error $ "No head synset found for " ++ show wordSense
        _           -> ("", "")


newtype SourcePosition = SourcePosition (Int, Int)
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)

-- synsets can be
data Unvalidated deriving (Binary,Generic)
data Validated

data Synset a = Synset
  { sourcePosition       :: SourcePosition
  , lexicographerFileId  :: LexicographerFileId
  , wordSenses           :: NonEmpty WNWord
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , relations            :: [SynsetRelation] -- [] use NonEmpty if not for a relationless adjectives?
  } deriving (Binary,Eq,Generic,Show)

instance Ord (Synset Validated) where
  Synset{wordSenses = (headWord:|_)} <= Synset{wordSenses = (headWord2:|_)}
    = headWord <= headWord2

---- validation
data Validation e a = Failure e | Success a deriving (Binary,Eq,Generic,Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Bifunctor Validation where
  bimap f _ (Failure e) = Failure (f e)
  bimap _ g (Success a) = Success (g a)

instance (Semigroup e, Semigroup a) => Semigroup (Validation e a) where
  Success s <> Success u = Success $ s <> u
  Success _ <> Failure f = Failure f
  Failure f <> Success _ = Failure f
  Failure f <> Failure a = Failure $ f <> a


instance Semigroup e => Applicative (Validation e) where
  --  pure :: a -> Validation e a
  pure = Success
  --(<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  Success f <*> Success a  = Success (f a)
  Success _ <*> Failure e  = Failure e
  Failure e <*> Success _  = Failure e
  Failure e <*> Failure e' = Failure (e <> e')

validate :: (a -> Validation e b) -> Validation e a -> Validation e b
validate f (Success a) = f a
validate _ (Failure e) = Failure e

validation :: (e -> b) -> (a -> b) -> Validation e a -> b
validation f _ (Failure e) = f e
validation _ g (Success a) = g a

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
  deriving (Binary,Generic,Show)

data SourceError
  = SourceError Text -- ^ name of source file
                SourcePosition
                WNError deriving (Binary,Generic,Show)

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

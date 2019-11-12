{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Data where

import Data.Aeson ( ToJSON(..) )
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (Binary)
import Data.Coerce (coerce)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ( Pretty(..),Doc,dot,colon,(<+>), nest, encloseSep
                                 , line, indent, align, vsep, squotes)
import GHC.Generics (Generic)


singleton :: a -> NonEmpty a
singleton x = x :| []

data WNObj = SynsetObj | WSenseObj deriving (Binary,Eq,Enum,Generic,Ord)

instance Show WNObj where
  show SynsetObj = "synset"
  show WSenseObj = "wordsense"

readWNObj :: Text -> WNObj
readWNObj input = case input of
  "synset" -> SynsetObj
  "word"   -> WSenseObj
  _        -> error . T.unpack
    $ T.intercalate " " ["Can't parse", input, "as WordNet object name (one of synset or word)"]

unsafeLookup :: Ord k => String -> k -> Map k a -> a
unsafeLookup errorMessage = M.findWithDefault $ error errorMessage

data WNPOS = A | S | R | N | V deriving (Binary,Eq,Enum,Generic,Ord,Read,Show,ToJSON)

readShortWNPOS :: (IsString a, Show a, Eq a) => a -> WNPOS
readShortWNPOS "n" = N
readShortWNPOS "a" = A
readShortWNPOS "r" = R
readShortWNPOS "v" = V
readShortWNPOS "s" = S
readShortWNPOS input = error $ show input ++ " is not a valid PoS"

readLongWNPOS :: Text -> Maybe WNPOS
readLongWNPOS "noun" = Just N
readLongWNPOS "verb" = Just V
readLongWNPOS "adjs" = Just S
readLongWNPOS "adj"  = Just A
readLongWNPOS "adv"  = Just R
readLongWNPOS _      = Nothing

showLongWNPOS :: WNPOS -> Text
showLongWNPOS N = "noun"
showLongWNPOS V = "verb"
showLongWNPOS R = "adv"
showLongWNPOS A = "adj"
showLongWNPOS S = "adj"

type LexName = Text

data LexicographerFileId
  = LexicographerFileId { pos     :: WNPOS
                        , lexname :: LexName
                        , wnName  :: WNName
                        }
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)

synsetType :: WNPOS -> Int
synsetType N = 1
synsetType V = 2
synsetType A = 3
synsetType R = 4
synsetType S = 5

lexicographerFileIdToText :: LexicographerFileId -> Text
lexicographerFileIdToText LexicographerFileId{pos, lexname} =
  T.concat [posText pos, ".", lexname]
  where
    posText N = "noun"
    posText V = "verb"
    posText A = "adj"
    posText S = "adjs"
    posText R = "adv"

newtype WordSenseForm = WordSenseForm Text
  deriving (Eq,Ord,Generic,Show)
  deriving newtype (Binary,Pretty,ToJSON)

tshow :: Show a => a -> Text
tshow = T.pack . show

type IdRelation = Maybe (RelationName, WordSenseForm)

type WNName = Text
type OneWN = Maybe WNName

data WNid =
  WNid { pos     :: WNPOS
       , lexname :: Text
       , wnName  :: WNName
       , lexForm :: WordSenseForm
       , idRel   :: IdRelation
       }
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary)

instance Pretty WNid where
  pretty WNid{pos, lexname, wnName, lexForm, idRel}
    =   pretty LexicographerFileId{pos, lexname, wnName}
    <>  colon
    <>  pretty lexForm
    <+> pretty idRel

instance ToJSON WNid where
  toJSON WNid{pos,lexname,wnName,lexForm,idRel}
    = toJSON (wnName, pos, lexname, lexForm, idRel)


toWNid :: (LexicographerFileId, WordSenseForm, IdRelation) -> WNid
toWNid (LexicographerFileId{pos,wnName,lexname}, lexForm, idRel) = WNid{pos,wnName,lexname,lexForm,idRel}

idLexFile :: WNid -> LexicographerFileId
idLexFile WNid{..} = LexicographerFileId{..}

newtype WordSenseId =
  WordSenseId WNid
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)
  deriving newtype (Pretty)

makeWordSenseId :: LexicographerFileId -> WordSenseForm -> IdRelation
  -> WordSenseId
makeWordSenseId LexicographerFileId{pos,lexname,wnName} lexForm idRel =
  WordSenseId WNid{pos,lexname,wnName,lexForm,idRel}

newtype SynsetId =
  SynsetId WNid
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)
  deriving newtype (Pretty)

type RelationName = Text
data Relation = Relation RelationName WNid
  deriving (Binary,Eq,Generic,Ord,Show,ToJSON)
newtype WordPointer = WordPointer Relation
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)
  deriving newtype (Pretty)
newtype SynsetRelation = SynsetRelation Relation
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)
  deriving newtype (Pretty)

type FrameId = Int
data WSense = WSense
  { wid      :: WordSenseId
  , extra    :: WNExtra
  , pointers :: [WordPointer]
  }
  deriving (Binary,Eq,Generic,Ord,Show)

-- senseKey :: Int -> Int -> Maybe SynsetRelation -> WSense -> String
-- senseKey lexFileNum synsetTypeNum maybeHeadRelation
--   wordSense@(WSense (WordSenseId WNid{lexForm = WordSenseForm wordForm,lexId = IdRelation lexicalId}) _ _)
--   = printf "%s%%%d:%02d:%02d:%s:%s" lemma synsetTypeNum lexFileNum lexicalId headWordForm (headWordIdRelation :: String)
--   where
--     lemma = T.toLower wordForm
--     (headWordForm, headWordIdRelation) =
--       case (synsetTypeNum, maybeHeadRelation) of
--         (5, Just (SynsetRelation _
--                   (SynsetId WNid{lexForm = WordSenseForm headForm,lexId = IdRelation headIdRelation})))
--           -> (T.toLower headForm, printf "%02d" headIdRelation)
--         (5,Nothing) -> error $ "No head synset found for " ++ show wordSense
--         _           -> ("", "")


newtype SourcePosition = SourcePosition (Int, Int)
  deriving (Eq,Generic,Ord,Show)
  deriving anyclass (Binary,ToJSON)

data SyntacticMarker = Attributive | Predicative | Postnominal
  deriving (Binary, Eq, Generic, Ord, Show)

data WNExtra = WNEmpty | WNVerb (NonEmpty FrameId) | WNAdj Text
  deriving (Binary, Eq, Generic, Ord, Show)

extraFrames :: WNExtra -> [FrameId]
extraFrames (WNVerb frames) = NE.toList frames
extraFrames _ = []

-- synsets can be
data Unvalidated deriving (Binary,Generic)
data Validated

data Synset a = Synset
  { comments            :: [Text]
  , sourcePosition      :: SourcePosition
  , lexicographerFileId :: LexicographerFileId
  , wordSenses          :: NonEmpty WSense
  , definition          :: Text
  , examples            :: [Text]
  , relations           :: [SynsetRelation] -- [] use NonEmpty if not for a relationless adjectives?
  , extra               :: WNExtra
  } deriving (Binary,Eq,Generic,Show)

instance Ord (Synset Validated) where
  Synset{wordSenses = (headWord:|_)} <= Synset{wordSenses = (headWord2:|_)}
    = headWord <= headWord2

synsetPOS :: Synset a -> WNPOS
synsetPOS Synset{lexicographerFileId = LexicographerFileId{pos}} = pos

synsetId :: Synset a -> SynsetId
synsetId Synset{wordSenses = WSense wordSenseId _ _:|_}
  = coerce wordSenseId

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

-- [] can refactor a lot of code involving relations by having
-- wordpointer and synsetrelation be newtypes of the same thing, and
-- using WNObj to distinguish between them when needed
data WNError
  = ParseError String -- [] do we still use all of these?
  | DuplicateWordSense String
  | DuplicateExamples (NonEmpty Text)
  | DuplicateFrames (NonEmpty FrameId)
  | DuplicateSynsetWords (NonEmpty WordSenseForm)
  | DuplicateWordRelation (NonEmpty WordPointer)
  | DuplicateSynsetRelation (NonEmpty SynsetRelation)
  | MissingRelationTarget WNObj Relation
  | AmbiguousRelationTarget WNObj Relation (NonEmpty (Synset Unvalidated))
  | UnsortedExamples (NonEmpty (NonEmpty Text))
  | UnsortedFrames (NonEmpty (NonEmpty FrameId))
  | UnsortedSynsets (NonEmpty (NonEmpty (Synset Validated)))
  | UnsortedWordSenses (NonEmpty (NonEmpty WordSenseForm))
  | UnsortedSynsetRelations  (NonEmpty (NonEmpty SynsetRelation))
  | UnsortedWordPointers (NonEmpty (NonEmpty WordPointer))
  | FramesNonVerb
  | MarkerNonAdj
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
instance Pretty WNObj where
  pretty SynsetObj = "synset"
  pretty WSenseObj = "wordsense"

instance Pretty WNPOS where
  pretty = pretty . showLongWNPOS

instance Pretty LexicographerFileId where
  pretty LexicographerFileId{pos, lexname, wnName} =
    "@" <> pretty wnName <> ":" <> pretty pos <> dot <> pretty lexname

instance Pretty Relation where
  pretty (Relation relationName wnId)
    = pretty relationName <> "»" <> pretty wnId

instance Pretty WSense where
  pretty (WSense (WordSenseId wnId) _ _)
    = pretty wnId

instance Pretty (Synset a) where
  pretty Synset{wordSenses = wordSense:|_} = pretty wordSense

prettyUnordered :: Pretty a => Text -> NonEmpty (NonEmpty a) -> Doc ann
prettyUnordered what sequences
  = "warning: Unsorted" <+> pretty what <> line
  <> (indent 2 . align . vsep . map prettyUnorderedSequence $ NE.toList sequences)
  where
    prettyUnorderedSequence (x:|xs) =
      squotes (pretty x) <+> "should come after"
      <+> encloseSep "" "" ", " (map (squotes . pretty) xs)

prettyDuplicate :: Pretty a => Text -> NonEmpty a -> Doc ann
prettyDuplicate what duplicates
  = "error: Duplicate"
  <+> pretty what
  <+> pretty (NE.head duplicates)

instance Pretty WNError where
  pretty (ParseError errorString) = pretty errorString
  pretty (DuplicateWordSense sensekey)
    = prettyDuplicate "wordsense" (singleton sensekey)
  pretty (DuplicateExamples examples)
    = prettyDuplicate "examples" examples
  pretty (DuplicateFrames frames)
    = prettyDuplicate "frame IDs" frames
  pretty (DuplicateSynsetWords synsetWords)
    = prettyDuplicate "synset words" synsetWords
  pretty (DuplicateWordRelation wordPointers)
    = prettyDuplicate "word pointer" wordPointers
  pretty (DuplicateSynsetRelation synsetRelations)
    = prettyDuplicate "synset relation" synsetRelations
  pretty (MissingRelationTarget wnObj (Relation relationName target))
    =   "error: Missing"
    <+> pretty relationName
    <+> pretty wnObj
    <+> "relation target" <+> pretty target
  pretty (AmbiguousRelationTarget wnObj (Relation relationName target) _)
    = "error: Ambiguous"
    <+> pretty wnObj
    <+> "relation"
    <+> pretty relationName
    <+> "with target"
    <+> pretty target
    -- [] could either be _
  pretty (UnsortedExamples sequences)
    = prettyUnordered "examples" sequences
  pretty (UnsortedFrames sequences)
    = prettyUnordered "synset IDs" sequences
  pretty (UnsortedSynsets sequences)
    = prettyUnordered "synsets" sequences
  pretty (UnsortedSynsetRelations sequences)
    = prettyUnordered "synset relations" sequences
  pretty (UnsortedWordSenses sequences)
    = prettyUnordered "synset word senses" sequences
  pretty (UnsortedWordPointers sequences)
    = prettyUnordered "word pointers" sequences
  pretty FramesNonVerb = "Can't have frames in non-verb synset/wordsense"
  pretty MarkerNonAdj  = "Can't have syntactic marker in non-adjective synset/wordsense"

instance Pretty SourceError where
  pretty (SourceError lexicographerFileId (SourcePosition (beg, end)) wnError)
    =   pretty lexicographerFileId
    <>  colon <> pretty beg <> colon <> pretty end <> colon
    <+> nest 2 (pretty wnError) <> line

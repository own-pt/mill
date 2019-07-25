module Validate where

import Data

import Data.Bifunctor (Bifunctor(..))
import Data.List hiding (insert)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie (Trie, fromList, member, foldWithKey, empty, insert)
import Data.Text.Prettyprint.Doc
  ( Pretty(..),Doc,(<+>), colon, align, hsep
  , emptyDoc, line, indent, vsep)

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?

type Index a = Trie String (Either String a) -- Left is a reference to another key

makeIndex :: [Synset Unvalidated] -> Index (Synset Unvalidated)
makeIndex synsets = fromList keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses = (headWordSense:|wordSenses)} =
      let headSenseKey = wordSenseKey headWordSense
      in
        (headSenseKey, Right synset) : map (\wordSense -> (wordSenseKey wordSense, Left headSenseKey)) wordSenses

wordSenseKey :: WNWord -> String
wordSenseKey (WNWord (WordSenseIdentifier (lexicographerFileId, wordForm, lexicalId)) _ _)
  = senseKey lexicographerFileId wordForm lexicalId

senseKey :: LexicographerFileId -> WordSenseForm -> LexicalId -> String
-- [ ] this is not really a sense key
senseKey  (LexicographerFileId (pos, lexname)) (WordSenseForm wordForm) lexicalId =
  intercalate "\t" [T.unpack wordForm, show pos ++ T.unpack lexname, show lexicalId]

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
  = SyntaxErrors
  | MissingSynsetRelationTarget SynsetRelation
  | MissingWordRelationTarget WordPointer
  | UnsortedSynsets (NonEmpty (NonEmpty (Synset Validated)))
  | UnsortedSynsetWordSenses (NonEmpty (NonEmpty WNWord))
  | UnsortedSynsetRelations  (NonEmpty (NonEmpty SynsetRelation))
  | UnsortedWordPointers (NonEmpty (NonEmpty WordPointer))
  deriving (Show)

data SourceError = SourceError LexicographerFileId SourcePosition WNError deriving (Show)

syntaxSourceErrors :: NonEmpty SourceError
syntaxSourceErrors = SourceError (LexicographerFileId (N, "placeholder"))
                                 (SourcePosition (0,0))
                     SyntaxErrors :| []

type WNValidation a = Validation (NonEmpty WNError) a
type SourceValidation a = Validation (NonEmpty SourceError) a

---
-- Pretty instances
prettyMissingTarget :: Text -> Text -> Doc ann -> Doc ann
prettyMissingTarget relationType relationName target
  =   "Missing"
  <+> pretty relationType
  <+> pretty relationName
  <+> "target" <+> target

prettyUnordered :: Pretty a => Text -> NonEmpty (NonEmpty a) -> Doc ann
prettyUnordered what sequences
  = "Unsorted" <+> pretty what <> line
  <> (indent 2 . align . vsep . map prettyUnorderedSequence $ NE.toList sequences)
  where
    prettyUnorderedSequence (x:|xs) =
      pretty x <+> "should come after" <+> hsep (map pretty xs)
  
instance Pretty WNError where
  pretty SyntaxErrors = emptyDoc
  pretty (MissingSynsetRelationTarget (SynsetRelation relationName target))
    = prettyMissingTarget "synset relation" relationName $ pretty target
  pretty (MissingWordRelationTarget (WordPointer pointerName target))
    = prettyMissingTarget "word pointer" pointerName $ pretty target
  pretty (UnsortedSynsets sequences)
    = prettyUnordered "synsets" sequences
  pretty (UnsortedSynsetRelations sequences)
    = prettyUnordered "synset relations" sequences
  pretty (UnsortedSynsetWordSenses sequences)
    = prettyUnordered "synset word senses" sequences
  pretty (UnsortedWordPointers sequences)
    = prettyUnordered "word pointers" sequences

instance Pretty SourceError where
  pretty (SourceError lexicographerFileId (SourcePosition (beg, end)) wnError)
    =   pretty lexicographerFileId
    <>  colon <> pretty beg <> colon <> pretty end <> colon
    <+> pretty wnError <> line

---
-- checks
checkSynset :: Index a -> Synset Unvalidated -> SourceValidation (Synset Validated)
checkSynset index Synset{lexicographerFileId, wordSenses, relations
                        , definition, examples, frames, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ NE.map (SourceError lexicographerFileId sourcePosition) errors
  where
    result = Synset
      <$> Success sourcePosition
      <*> Success lexicographerFileId
      <*> checkWordSenses index wordSenses
      <*> Success definition
      <*> Success examples
      <*> Success frames -- [ ] check frames
      <*> checkSynsetRelationsTargets index relations


--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

checkSynsetRelations :: Index a -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelations index synsetRelations
  =  checkSynsetRelationsTargets index synsetRelations
  *> checkSynsetRelationsOrder synsetRelations
  *> Success synsetRelations

checkSynsetRelationsOrder :: [SynsetRelation] -> WNValidation [SynsetRelation]
checkSynsetRelationsOrder synsetRelations
  = bimap (\errs -> UnsortedSynsetRelations errs :| []) id
  $ validateSorted synsetRelations

checkSynsetRelationsTargets :: Index a -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelationsTargets index = traverse checkSynsetRelation
  where
    checkSynsetRelation synsetRelation@(SynsetRelation _ (SynsetIdentifier (lexFileId, wordForm, lexicalId))) =
      if member targetSenseKey index
      then Success synsetRelation
      else Failure (MissingSynsetRelationTarget synsetRelation :| []) -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

validateSorted :: Ord a => [a] -> Validation (NonEmpty (NonEmpty a)) [a]
-- maybe just sort input instead of picking some of the errors?
--- or maybe just check if every pair is sorted, which will be faster
--- but might take more iterations to find all errors
validateSorted [] = Success []
validateSorted [x] = Success [x]
validateSorted (x:y:xt)
  | x <= y    = (:) <$> Success x <*> validateSorted (y:xt)
  | otherwise = let (wrongs, _) = span (< x) xt
                in (:) <$> Failure ((x:|(y:wrongs)):|[])
                       <*> validateSorted (y:xt)

checkWordSenses :: Index a -> NonEmpty WNWord -> WNValidation (NonEmpty WNWord)
checkWordSenses index wordSenses =
  checkWordSensesOrder wordSenses *>
  checkWordSensesPointerTargets index wordSensesPointers *>
  validatedWordSensesPointersOrder *>
  Success wordSenses
  where
    wordSensesPointers = concatMap (\(WNWord _ _ wordPointers) -> wordPointers) wordSenses
    validatedWordSensesPointersOrder = bimap (\errs -> UnsortedWordPointers errs :| []) id
      $ validateSorted wordSensesPointers

checkWordSensesPointerTargets :: Index a -> [WordPointer]
  -> WNValidation [WordPointer]
checkWordSensesPointerTargets index = traverse checkWordPointer
  where
    checkWordPointer wordPointer@(WordPointer _ (WordSenseIdentifier (lexFileId, wordForm, lexicalId))) =
      -- check pointer name too
      if member targetSenseKey index
      then Success wordPointer
      else Failure (MissingWordRelationTarget wordPointer :| []) -- []
      where
        targetSenseKey = senseKey lexFileId wordForm lexicalId

checkWordSensesOrder :: NonEmpty WNWord -> WNValidation [WNWord]
checkWordSensesOrder = bimap (\errs -> UnsortedSynsetWordSenses errs :| []) id
  . validateSorted . NE.toList

--- https://www.reddit.com/r/haskell/comments/6zmfoy/the_state_of_logging_in_haskell/

validateIndex :: Index (Synset Unvalidated) -> SourceValidation (Index (Synset Validated))
-- [ ] not validating if there are two things with the same reference
validateIndex index = foldWithKey go (Success empty) index
  where
    go key (Left headWordKey) result = insert key (Left headWordKey) <$> result
    go key (Right synset) result = insert key . Right <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index


validateSynsetsInIndex :: Index (Synset Unvalidated)
  -> SourceValidation [Synset Validated]
validateSynsetsInIndex index = foldWithKey go (Success []) index
  where
    go _ (Left _) result = result
    go _ (Right synset) result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

validateSynsets :: Index (Synset Unvalidated) -> [Synset Unvalidated]
  -> SourceValidation [Synset Validated]
validateSynsets index synsets =
  checkSynsetsOrder checkedSynsets
  where
    checkSynsetsOrder (Success validatedSynsets)
      = bimap (NE.map toSourceError) id $ validateSorted validatedSynsets
    checkSynsetsOrder (Failure es) = Failure es
    toSourceError errs@(Synset{sourcePosition, lexicographerFileId} :| _)
      = SourceError lexicographerFileId sourcePosition
          (UnsortedSynsets (errs :| []))
    checkedSynsets = foldr go (Success []) synsets
    go synset result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

---


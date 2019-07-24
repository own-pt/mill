module Validate where

import Data

import Data.Bifunctor (Bifunctor(..))
import Data.List hiding (insert)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.GenericTrie (Trie, fromList, member, foldWithKey, empty, insert)

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
wordSenseKey (WNWord (WordSenseIdentifier (lexicographerFileId, wordForm, lexicalId)) _ _) =
  senseKey lexicographerFileId wordForm lexicalId

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

validation :: (e -> b) -> (a -> b) -> Validation e a -> b
validation f _ (Failure e) = f e
validation _ g (Success a) = g a

data WNError
  = SyntaxErrors
  | MissingSynsetRelationTarget SynsetRelation
  | UnsortedSynsetRelations  (NonEmpty (NonEmpty SynsetRelation))
  | MissingWordRelationTarget WordPointer
  | UnsortedSynsetWordSenses (NonEmpty (NonEmpty WNWord))
  | UnsortedWordPointers (NonEmpty (NonEmpty WordPointer))
  deriving (Show)

data SourceError = SourceError LexicographerFileId SourcePosition WNError deriving (Show)

syntaxSourceErrors :: NonEmpty SourceError
syntaxSourceErrors = SourceError (LexicographerFileId (N, "placeholder"))
                                 (SourcePosition (0,0))
                     SyntaxErrors :| []

type WNValidation a = Validation (NonEmpty WNError) a
type SourceValidation a = Validation (NonEmpty SourceError) a

showSourceError :: SourceError -> Text
-- use http://hackage.haskell.org/package/formatting-6.3.7/docs/Formatting.html ?
showSourceError (SourceError lexicographerFileId (SourcePosition pos) wnError) =
  T.concat
  [ lexicographerFileIdToText lexicographerFileId
  , ":", T.pack $ show pos, ": "
  , showWNError wnError
  ]
  where
    showWNError SyntaxErrors = "" -- already reported by Megaparsec
    showWNError (MissingSynsetRelationTarget
                 (SynsetRelation relationName synsetId)) =
      T.concat ["Missing ", relationName, " relation target ", showSynsetId synsetId]
    showWNError (UnsortedSynsetRelations _) = "placeholder"
    showWNError (MissingWordRelationTarget
                 (WordPointer pointerName wordSenseId)) = T.concat ["Missing ", pointerName, " word relation target ", showWordSenseId wordSenseId]
    showWNError (UnsortedSynsetWordSenses _) = T.concat ["Unsorted word senses; order should be "]
    showWNError (UnsortedWordPointers _) = "Unsorted word pointers"
    showWordSenseId (WordSenseIdentifier wordSenseIdentifier) = showIdentifier wordSenseIdentifier
    showSynsetId (SynsetIdentifier synsetIdentifier) = showIdentifier synsetIdentifier
    showIdentifier (lexicographerId, WordSenseForm wordForm, LexicalId lexicalId) =
      T.concat
      [ wordForm, ":"
      , T.pack . show $ lexicalId, " at file "
      , lexicographerFileIdToText lexicographerId
      ]

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
checkSynsetRelations index synsetRelations =
  checkSynsetRelationsTargets index synsetRelations
  *> checkSynsetRelationsOrder synsetRelations
  *> Success synsetRelations

checkSynsetRelationsOrder :: [SynsetRelation] -> WNValidation [SynsetRelation]
checkSynsetRelationsOrder synsetRelations = bimap (\errs -> UnsortedSynsetRelations errs :| []) id $ validateSorted synsetRelations

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
validateSorted [] = Success []
validateSorted [x] = Success [x]
validateSorted (x:y:xt)
  | x <= y    = (:) <$> Success x <*> validateSorted (y:xt)
  | otherwise = let (wrongs, rest) = span (< x) xt
                in (:) <$> Failure ((x:|(y:wrongs)):|[])
                       <*> validateSorted rest

checkList :: Semigroup e => (b -> Validation e a) -> [b] -> Validation e [a]
checkList _ []     = Success []
checkList f (x:xs) = (:) <$> f x <*> checkList f xs

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

checkWordSensesOrder :: NonEmpty WNWord -> Validation (NonEmpty WNError) [WNWord]
checkWordSensesOrder = bimap (\errs -> UnsortedSynsetWordSenses errs :| []) id . validateSorted . NE.toList

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
validateSynsets index = foldr go (Success [])
  where
    go synset result = (:) <$> checkSynset' synset <*> result
    checkSynset' = checkSynset index

---


{-# LANGUAGE FlexibleContexts #-}
module Validate
  ( validateSynsets
  , ValIndex
  , indexSynsets
  , lookupIndex
  , makeIndex
  , oneLangIndex
  , Index
  , indexKey
  , wordSenseKey
  ) where

import Data (Synset(..), Unvalidated, Validated, SourceValidation
            , WSense(..), WNid(..), Validation(..), WordSenseForm(..)
            , SynsetRelation(..),WordPointer(..), WNValidation, WNError(..), WNPOS(..)
            , LexicographerFileId(..), Relation(..), WNObj(..)
            , WordSenseId(..),SourceError(..), WNExtra(..), singleton,lexicographerFileIdToText)

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.ListTrie.Base.Map (WrappedIntMap)
import Data.ListTrie.Patricia.Map (TrieMap,fromListWith'
                                  ,toAscList,map',lookup)
import Prelude hiding (lookup)

--- idea: wordsenses are identified by their lexfile, lexical form,
--- and (if necessary) an unambiguous relation + lexform. we start
--- with a validation index which maps (lexfile, lexical form) to
--- synsets; we then check that all references are unique; while doing
--- so we add a temporary meaningless id to each reference (lexfile,
--- sourcePosition) which allows us to build the final index, which is
--- used by the rest of the application

--- having two indices will not work because it would hinder
--- incremental validation (only validating one file against several)

--- although we don't really need two indices (only for wndb export,
--- or for finding all references to an id [not implemented yet])

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?
type Index a     = TrieMap WrappedIntMap Char (Either String a)
type ValIndex a = TrieMap WrappedIntMap Char (NonEmpty a)

makeIndex :: NonEmpty (Synset Unvalidated) -> ValIndex (Synset Unvalidated)
makeIndex synsets = fromListWith' (<>) keyValuePairs
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses} =
      NE.toList $
      NE.map ((, singleton synset) . wordSenseKey)
      wordSenses

--- the whole concept of duplicates has changed, now it's basically
--- two indistinguishable synsets; so this is uneeded in its present
--- form
-- checkIndexNoDuplicates :: ValIndex (Synset Unvalidated)
--   -> SourceValidation (Index (Synset Unvalidated))
-- checkIndexNoDuplicates index =
--   case mapAccumWithKey' go [] index of
--     ([], indexNoDuplicates) -> Success indexNoDuplicates
--     (x:duplicateErrors, _)  -> Failure $ x:|duplicateErrors
--   where
--     go duplicateErrors _ (value :| [])
--       = (duplicateErrors, value)
--     go duplicateErrors key values
--       = ( moreDuplicateErrors ++ duplicateErrors
--         , NE.head values )
--       where
--         lookupKey headKey = case lookup headKey index of
--           Just values' ->
--             head . filter (elem key . NE.map wordSenseKey . wordSenses) . rights
--             $ NE.toList values'
--           Nothing -> error $ "Missing synset to key " ++ key
--         moreDuplicateErrors
--           = map (\value -> toSourceError (either lookupKey id value)
--                 . DuplicateWordSense $ takeWhile (/= '\t') key)
--             $ NE.toList values

wordSenseKey :: WSense -> String
wordSenseKey (WSense wnWordId _ _)
  = indexKey $ coerce wnWordId

indexKey :: WNid -> String
-- this is NOT the sensekey
indexKey WNid{wnName, pos, lexname, lexForm = WordSenseForm wordForm} =
  -- if changing this definition change WNDB export too
  intercalate "\t" [ T.unpack wnName
                   , show pos ++ T.unpack lexname
                   , T.unpack wordForm
                   ]


---
-- checks
checkSynset :: ValIndex (Synset Unvalidated) -> Synset Unvalidated -> SourceValidation (Synset Validated)
checkSynset index Synset{comments, lexicographerFileId, wordSenses, relations
                        , definition, examples, extra, sourcePosition} =
  case result of
    Success synset -> Success synset
    Failure errors -> Failure $ NE.map (SourceError lexfileName sourcePosition) errors
  where
    lexfileName = lexicographerFileIdToText lexicographerFileId
    synsetPOS = pos (lexicographerFileId :: LexicographerFileId)
    result = Synset
      <$> Success comments
      <*> Success sourcePosition
      <*> Success lexicographerFileId
      <*> checkWordSenses index wordSenses
      <*> Success definition
      <*> checkSortNoDuplicates UnsortedExamples DuplicateExamples examples
      <*> checkSynsetRelations index relations
      -- actually can't have syntactic marker in synset, but parser
      -- won't allow it anyway
      <*> checkExtra synsetPOS extra


--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

checkSynsetRelations :: ValIndex (Synset Unvalidated) -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelations index synsetRelations
  =  checkSynsetRelationsOrderNoDuplicates
  *> checkSynsetRelationsTargets index synsetRelations
  *> Success synsetRelations
  where
    checkSynsetRelationsOrderNoDuplicates
      = checkSortNoDuplicates UnsortedSynsetRelations DuplicateSynsetRelation synsetRelations

checkSynsetRelationsTargets :: ValIndex (Synset Unvalidated) -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
-- we check that the relation target exists, and more importantly that
-- it is unique
checkSynsetRelationsTargets index relations
  = coerce
  . checkRelationsTargets index SynsetObj
  $ coerce relations

validateSorted :: Ord a => [a] -> Validation (NonEmpty (NonEmpty a)) [a]
-- maybe just sort input instead of picking some of the errors?
--- or maybe just check if every pair is sorted, which will be faster
--- but will take more iterations to find all errors
validateSorted (x:y:xt)
  | x <= y    = (:) <$> Success x <*> validateSorted (y:xt)
  | otherwise = let (wrongs, _) = span (< x) xt
                in (:) <$> Failure ((x:|(y:wrongs)):|[])
                       <*> validateSorted (y:xt)
validateSorted x = Success x

validateNoDuplicates :: Ord a => [a] -> Validation (NonEmpty a) [a]
validateNoDuplicates (x:y:xt)
  | x < y  = (:) <$> Success x <*> validateNoDuplicates (y:xt)
  | x == y = let (equals, rest) = span (== x) xt
             in (:) <$> Failure (x:|y:equals) <*> validateNoDuplicates rest
  | x > y  = error "Unsorted"
validateNoDuplicates x = Success x

checkSortNoDuplicates
  :: Ord a
  => (NonEmpty (NonEmpty a) -> WNError)
  -> (NonEmpty a -> WNError)
  -> [a]
  -> WNValidation [a]
checkSortNoDuplicates toSortError toDuplicateError = sortedCheckNoDuplicates . validateSorted
  where
    sortedCheckNoDuplicates (Failure unsortedSequences)
      = Failure (singleton $ toSortError unsortedSequences)
    sortedCheckNoDuplicates (Success xs)
      = bimap (singleton . toDuplicateError) id $ validateNoDuplicates xs

checkWordSenses :: ValIndex (Synset Unvalidated) -> NonEmpty WSense -> WNValidation (NonEmpty WSense)
checkWordSenses index wordSenses
  =  checkWordSensesOrderNoDuplicates
  *> traverse (checkWordSense index) wordSenses
  *> Success wordSenses
  where
    checkWordSensesOrderNoDuplicates
      = checkSortNoDuplicates UnsortedWordSenses DuplicateSynsetWords . map wordSenseLexicalForm $ NE.toList wordSenses
    wordSenseLexicalForm (WSense (WordSenseId WNid{lexForm}) _ _) = lexForm

checkWordSense :: ValIndex (Synset Unvalidated) -> WSense -> WNValidation WSense
checkWordSense index wordSense@(WSense wID extra wordPointers)
  =  checkWordSensePointersOrderNoDuplicates
  *> checkWordSensePointersTargets index wordPointers
  *> checkExtra wordPOS extra
  *> Success wordSense
  where
    wordPOS = pos (coerce wID :: WNid)
    checkWordSensePointersOrderNoDuplicates =
      checkSortNoDuplicates UnsortedWordPointers DuplicateWordRelation wordPointers
    

checkWordSensePointersTargets :: ValIndex (Synset Unvalidated) -> [WordPointer]
  -> WNValidation [WordPointer]
checkWordSensePointersTargets index relations
  = coerce
  . checkRelationsTargets index WSenseObj
  $ coerce relations

checkRelationsTargets :: ValIndex (Synset Unvalidated) -> WNObj -> [Relation]
  -> WNValidation [Relation]
checkRelationsTargets index wnObj = traverse checkRelation
  where
    checkRelation relation@(Relation _ wnid@WNid{idRel}) =
      checkCandidates maybeCandidates idRel
      where
        targetSenseKey = indexKey wnid
        maybeCandidates = maybe [] NE.toList $ lookup targetSenseKey index
        ambiguityError = Failure . singleton . AmbiguousRelationTarget wnObj relation 
        missingRelationError = Failure . singleton $ MissingRelationTarget wnObj relation
        checkCandidates []  _ = missingRelationError
        checkCandidates [_] _ = Success relation
        checkCandidates (x:candidates) Nothing = ambiguityError (x:|candidates)
        checkCandidates candidates (Just (relName, targetLexForm))
          = case filter isTarget candidates of
              [] -> missingRelationError
              [_] -> Success relation
              x:xs -> ambiguityError $ x:|xs
          where
            isTarget Synset{relations, wordSenses}
              = any relationIsTarget relations
              || (any pointerIsTarget . concatMap pointers $ NE.filter targetWord wordSenses)
              where
                relationIsTarget (SynsetRelation (Relation name WNid{lexForm}))
                  = name == relName && lexForm == targetLexForm
                pointerIsTarget (WordPointer (Relation name WNid{lexForm}))
                  = name == relName && lexForm == targetLexForm
                targetWord (WSense (WordSenseId WNid{lexForm}) _ _) = lexForm == targetLexForm
        

checkExtra :: WNPOS -> WNExtra -> WNValidation WNExtra
checkExtra _ WNEmpty = Success WNEmpty
checkExtra A (WNAdj marker) = Success (WNAdj marker)
checkExtra S (WNAdj marker) = Success (WNAdj marker)
checkExtra V (WNVerb frames) = fmap WNVerb checkedFrames
  where
    checkedFrames = fmap NE.fromList . checkSortNoDuplicates UnsortedFrames DuplicateFrames $ NE.toList frames
checkExtra _ (WNAdj _) = Failure $ singleton MarkerNonAdj
checkExtra _ (WNVerb _) = Failure $ singleton FramesNonVerb

validateSynsets :: ValIndex (Synset Unvalidated)
  -> NonEmpty (Synset Unvalidated)
  -> SourceValidation (NonEmpty (Synset Validated))
-- | validates synsets from the same lexicographer file against index
validateSynsets index (firstSynset:|synsets) =
  (:|)
  <$> checkSynset' firstSynset
  <*> foldr go (Success []) synsets
  where
    checkSynset' = checkSynset index
    go synset result = (:) <$> checkSynset' synset <*> result


indexSynsets :: Index (Synset Unvalidated) -> [Synset Unvalidated]
indexSynsets = concatMap (either (const []) (:[]) . snd) . toAscList

oneLangIndex :: Maybe Text -> Index (Synset Unvalidated) -> Index (Synset Unvalidated)
-- | Remove from the index all relations pointing to synsets in other
-- WNs; the index should not have any synsets from other WNs. in
-- normal operation this doesn't happen because these synsets do not
-- even get read when one language is specified
oneLangIndex Nothing = id
oneLangIndex (Just lang)
  = map' go
  where
    go (Left headKey) = Left headKey
    go (Right s@Synset{wordSenses,relations})
      = Right s{relations = filter oneLang relations, wordSenses = NE.map oneLangWordSense wordSenses}
      where
        oneLang (SynsetRelation (Relation _ WNid{wnName})) = wnName == lang
        oneLangWordSense (WSense wID fs pointers)
          = WSense wID fs
          $ filter oneLangPointer pointers
        oneLangPointer (WordPointer (Relation _ WNid{wnName})) = wnName == lang

---

lookupIndex :: String -> Index (Synset a) -> Maybe (Synset a)
lookupIndex key index =
  case lookup key index of
    Just (Left headKey) -> lookupIndex headKey index
    Just (Right synset) -> Just synset
    Nothing -> Nothing

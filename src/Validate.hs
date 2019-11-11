{-# LANGUAGE FlexibleContexts #-}
module Validate
  ( validateSynsets
  , ValIndex
  , SynsetMap
  , mapSynsets
  , lookupIndex
  , makeIndex
  , removeInterWNRelations
  , Index
  , indexKey
  , wordSenseKey
  ) where

import Data (Synset(..), Unvalidated, Validated, SourceValidation, WNName
            , WSense(..), WNid(..), Validation(..), WordSenseForm(..)
            , SynsetRelation(..),WordPointer(..), WNValidation, WNError(..), WNPOS(..)
            , LexicographerFileId(..), Relation(..), WNObj(..), SourcePosition(..)
            , WordSenseId(..),SourceError(..), WNExtra(..), OneWN, singleton,lexicographerFileIdToText)

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.ListTrie.Base.Map (WrappedIntMap)
import Data.ListTrie.Patricia.Map (TrieMap,fromListWith'
                                  ,lookup)
import Prelude hiding (lookup)

-- [] use Reader monad so that we don't need to pass arguments
-- everywhere

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?
type Index a     = TrieMap WrappedIntMap Char (Either String a)
type SynsetKey = (WNName, WNPOS, Text, Int)
type SynsetMap a = Map SynsetKey (Synset a)
type ValIndex = TrieMap WrappedIntMap Char (NonEmpty SynsetKey)

makeIndex :: NonEmpty (Synset Unvalidated) -> (SynsetMap Unvalidated, ValIndex)
makeIndex synsets = (synsetMap, fromListWith' (<>) keyValuePairs)
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses} =
      NE.toList $
      NE.map ((, singleton $ synsetKey synset) . wordSenseKey)
      wordSenses
    synsetMap = M.fromList . NE.toList $ NE.zip (NE.map synsetKey synsets) synsets
    synsetKey Synset{sourcePosition = SourcePosition (beg, _), lexicographerFileId = LexicographerFileId{lexname, pos, wnName}}
      = (wnName, pos, lexname, beg)

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
checkSynset :: ValIndex -> SynsetMap Unvalidated -> Synset Unvalidated -> SourceValidation (Synset Validated)
checkSynset index synsetMap
  Synset{comments, lexicographerFileId, wordSenses, relations
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
      <*> checkWordSenses index synsetMap wordSenses
      <*> Success definition
      <*> checkSortNoDuplicates UnsortedExamples DuplicateExamples examples
      <*> checkSynsetRelations index synsetMap relations
      -- actually can't have syntactic marker in synset, but parser
      -- won't allow it anyway
      <*> checkExtra synsetPOS extra


--- use <*> for validation, or <*? see
--- https://www.reddit.com/r/haskell/comments/7hqodd/pure_functional_validation/

checkSynsetRelations :: ValIndex -> SynsetMap Unvalidated
  -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
checkSynsetRelations index synsetMap synsetRelations
  =  checkSynsetRelationsOrderNoDuplicates
  *> checkSynsetRelationsTargets index synsetMap synsetRelations
  *> Success synsetRelations
  where
    checkSynsetRelationsOrderNoDuplicates
      = checkSortNoDuplicates UnsortedSynsetRelations DuplicateSynsetRelation synsetRelations

checkSynsetRelationsTargets :: ValIndex -> SynsetMap Unvalidated -> [SynsetRelation]
  -> WNValidation [SynsetRelation]
-- we check that the relation target exists, and more importantly that
-- it is unique
checkSynsetRelationsTargets index synsetMap relations
  = coerce
  . checkRelationsTargets index synsetMap SynsetObj
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

checkWordSenses :: ValIndex -> SynsetMap Unvalidated -> NonEmpty WSense -> WNValidation (NonEmpty WSense)
checkWordSenses index synsetMap wordSenses
  =  checkWordSensesOrderNoDuplicates
  *> traverse (checkWordSense index synsetMap) wordSenses
  *> Success wordSenses
  where
    checkWordSensesOrderNoDuplicates
      = checkSortNoDuplicates UnsortedWordSenses DuplicateSynsetWords . map wordSenseLexicalForm $ NE.toList wordSenses
    wordSenseLexicalForm (WSense (WordSenseId WNid{lexForm}) _ _) = lexForm

checkWordSense :: ValIndex -> SynsetMap Unvalidated -> WSense -> WNValidation WSense
checkWordSense index synsetMap wordSense@(WSense wID extra wordPointers)
  =  checkWordSensePointersOrderNoDuplicates
  *> checkWordSensePointersTargets index synsetMap wordPointers
  *> checkExtra wordPOS extra
  *> Success wordSense
  where
    wordPOS = pos (coerce wID :: WNid)
    checkWordSensePointersOrderNoDuplicates =
      checkSortNoDuplicates UnsortedWordPointers DuplicateWordRelation wordPointers


checkWordSensePointersTargets :: ValIndex -> SynsetMap Unvalidated -> [WordPointer]
  -> WNValidation [WordPointer]
checkWordSensePointersTargets index synsetMap relations
  = coerce
  . checkRelationsTargets index synsetMap WSenseObj
  $ coerce relations

checkRelationsTargets :: ValIndex -> SynsetMap Unvalidated -> WNObj -> [Relation]
  -> WNValidation [Relation]
checkRelationsTargets index synsetMap wnObj = traverse checkRelation
  where
    checkRelation relation@(Relation _ wnid@WNid{lexForm = targetLexForm, idRel}) =
      checkCandidates maybeCandidates idRel
      where
        maybeCandidates = lookupIndex targetSenseKey index synsetMap
        targetSenseKey = indexKey wnid
        ambiguityError = Failure . singleton . AmbiguousRelationTarget wnObj relation
        missingRelationError = Failure . singleton $ MissingRelationTarget wnObj relation
        checkCandidates []  _ = missingRelationError
        checkCandidates [_] Nothing = Success relation
        checkCandidates (x:candidates) Nothing = ambiguityError (x:|candidates)
        checkCandidates candidates (Just ("syn", synonymLexForm))
        -- [] unhardcode this name; either parse to graph already
        -- (probs best option) or add synonym relations
          = case filter isTarget candidates of
              [] -> missingRelationError
              [_] -> Success relation
              x:xs -> ambiguityError $ x:|xs
          where
            isTarget Synset{wordSenses}
              = any targetWord wordSenses
              where
                targetWord (WSense (WordSenseId WNid{lexForm}) _ _)
                  = lexForm == synonymLexForm
        checkCandidates candidates (Just (relName, idRelTargetLexForm))
          = case filter isTarget candidates of
              [] -> missingRelationError
              [_] -> Success relation
              x:xs -> ambiguityError $ x:|xs
          where
            isTarget Synset{relations, wordSenses}
              = any (relationIsTarget . coerce) relations
              || any (relationIsTarget . coerce) (concatMap pointers targetSenses)
              where
                relationIsTarget (Relation name WNid{lexForm})
                  = name == relName && lexForm == idRelTargetLexForm
                targetWord (WSense (WordSenseId WNid{lexForm}) _ _)
                  = lexForm == targetLexForm
                targetSenses -- actually there should only be one; if
                             -- there's more validation will catch it
                  = NE.filter targetWord wordSenses



checkExtra :: WNPOS -> WNExtra -> WNValidation WNExtra
checkExtra _ WNEmpty = Success WNEmpty
checkExtra A (WNAdj marker) = Success (WNAdj marker)
checkExtra S (WNAdj marker) = Success (WNAdj marker)
checkExtra V (WNVerb frames) = fmap WNVerb checkedFrames
  where
    checkedFrames = fmap NE.fromList . checkSortNoDuplicates UnsortedFrames DuplicateFrames $ NE.toList frames
checkExtra _ (WNAdj _) = Failure $ singleton MarkerNonAdj
checkExtra _ (WNVerb _) = Failure $ singleton FramesNonVerb

validateSynsets :: ValIndex
  -> SynsetMap Unvalidated
  -> NonEmpty (Synset Unvalidated)
  -> SourceValidation (NonEmpty (Synset Validated))
-- | validates synsets from the same lexicographer file against index
validateSynsets index synsetMap (firstSynset:|synsets) =
  (:|)
  <$> checkSynset' firstSynset
  <*> foldr go (Success []) synsets
  where
    checkSynset' = checkSynset index synsetMap
    go synset result = (:) <$> checkSynset' synset <*> result


mapSynsets :: SynsetMap a -> [Synset a]
mapSynsets = M.elems

removeInterWNRelations :: OneWN -> SynsetMap a -> SynsetMap a
-- | Remove from the synsets all relations pointing to synsets in
-- other WNs; there should be no synset from other WNs among the
-- input. in normal operation this doesn't happen because these
-- synsets do not even get read when one language is specified
removeInterWNRelations Nothing = id
removeInterWNRelations (Just lang)
  = M.map go
  where
    go s@Synset{wordSenses,relations}
      = s{relations = filter (sameWN . coerce) relations, wordSenses = NE.map oneWNWordSense wordSenses}
      where
        oneWNWordSense (WSense wID fs pointers)
          = WSense wID fs
          $ filter (sameWN . coerce) pointers
        sameWN (Relation _ WNid{wnName}) = wnName == lang

---

lookupIndex :: String -> ValIndex -> SynsetMap a -> [Synset a]
lookupIndex key index synsetMap =
  case lookup key index of
    Just synsetKeys -> catMaybes . NE.toList $ NE.map (`M.lookup` synsetMap) synsetKeys
    Nothing -> []

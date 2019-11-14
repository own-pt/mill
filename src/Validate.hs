{-# LANGUAGE FlexibleContexts #-}
module Validate where

import Data (Synset(..), Unvalidated, Validated, SourceValidation, SynsetKey
            , WSense(..), WNid(..), Validation(..), LexicalForm(..)
            , SynsetRelation(..),WordPointer(..), WNValidation, WNError(..), WNPOS(..)
            , LexicographerFileId(..), Relation(..), WNObj(..), LexName, WNName
            ,SourceError(..), WNExtra(..), OneWN
            , singleton, lexicographerFileIdToText, synsetKey, unsafeLookup)

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.List hiding (insert, lookup)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.ListTrie.Base.Map (WrappedIntMap)
import Data.ListTrie.Patricia.Map (TrieMap,fromListWith'
                                  ,lookup)
import Prelude hiding (lookup)

-- [] use Reader monad so that we don't need to pass arguments
-- everywhere

-- when to change LexicographerFile : Text to LexicographerFileId :
-- Int in wordsenses etc.? is changing it really necessary?
type SynsetMap a = Map SynsetKey (Synset a)
type ValIndex = TrieMap WrappedIntMap Char (NonEmpty SynsetKey)

makeIndex :: NonEmpty (Synset Unvalidated) -> (SynsetMap Unvalidated, ValIndex)
makeIndex synsets = (synsetMap, fromListWith' (<>) keyValuePairs)
  where
    keyValuePairs = concatMap synsetPairs synsets
    synsetPairs synset@Synset{wordSenses, lexicographerFileId} =
      NE.toList $
      NE.map ((, singleton $ synsetKey synset) . wordSenseKey lexicographerFileId)
      wordSenses
    synsetMap = M.fromList . NE.toList $ NE.zip (NE.map synsetKey synsets) synsets

wordSenseKey :: LexicographerFileId -> WSense -> String
wordSenseKey LexicographerFileId{pos, lexname, wnName} WSense{lexicalForm}
  = indexKey wnName pos lexname lexicalForm

indexKey :: WNName -> WNPOS -> LexName -> LexicalForm -> String
-- this is NOT the sensekey
indexKey wnName pos lexname (LexicalForm lexForm) =
  -- if changing this definition change WNDB export too
  intercalate "\t" [ T.unpack wnName
                   , show pos ++ T.unpack lexname
                   , T.unpack lexForm
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
      <*> checkWordSenses index synsetMap  synsetPOS wordSenses
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

checkWordSenses :: ValIndex -> SynsetMap Unvalidated -> WNPOS -> NonEmpty WSense -> WNValidation (NonEmpty WSense)
checkWordSenses index synsetMap pos wordSenses
  =  checkWordSensesOrderNoDuplicates
  *> traverse (checkWordSense index synsetMap pos) wordSenses
  *> Success wordSenses
  where
    checkWordSensesOrderNoDuplicates
      = checkSortNoDuplicates UnsortedWordSenses DuplicateSynsetWords . map wordSenseLexicalForm $ NE.toList wordSenses
    wordSenseLexicalForm WSense{lexicalForm} = lexicalForm

checkWordSense :: ValIndex -> SynsetMap Unvalidated -> WNPOS -> WSense -> WNValidation WSense
checkWordSense index synsetMap wordPOS wordSense@(WSense _ extra wordPointers)
  =  checkWordSensePointersOrderNoDuplicates
  *> checkWordSensePointersTargets index synsetMap wordPointers
  *> checkExtra wordPOS extra
  *> Success wordSense
  where
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
    checkRelation relation@(Relation _ wnid) =
      case lookupSenseCandidates wnid index synsetMap of
        [] -> missingRelationError
        [_] -> Success relation
        (x:candidates) -> ambiguityError (x:|candidates)
      where
        ambiguityError = Failure . singleton . AmbiguousRelationTarget wnObj relation
        missingRelationError = Failure . singleton $ MissingRelationTarget wnObj relation

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

lookupSynset :: SynsetMap a -> SynsetKey -> Synset a
lookupSynset synsetMap synsetID =
  unsafeLookup ("No synset correponding to " ++ show synsetID) synsetID synsetMap

lookupSenseCandidates :: WNid -> ValIndex -> SynsetMap a -> [Synset a]
-- | find synsets corresponding to human-readable id, if any
lookupSenseCandidates WNid{wnName, pos, lexname, lexForm, idRel} index synsetMap =
  findTarget idRel
  where
    candidates = lookupIndex (indexKey wnName pos lexname lexForm) index synsetMap
    findTarget (Just ("syn", synonymLexForm)) =
      -- [] unhardcode this name; either parse to graph already
      -- (probs best option) or add synonym relations
      filter isTarget candidates
      where
        isTarget Synset{wordSenses}
          = any targetWord wordSenses
          where
            targetWord WSense{lexicalForm = otherLexForm}
              = otherLexForm == synonymLexForm
    findTarget (Just (relName, idRelTargetLexForm))
      = filter isTarget candidates
      where
        isTarget Synset{relations, wordSenses}
          = any (relationIsTarget . coerce) relations
          || any (relationIsTarget . coerce) (concatMap pointers targetSenses)
          where
            relationIsTarget (Relation name WNid{lexForm = otherLexForm})
              = name == relName && otherLexForm == idRelTargetLexForm
            targetWord WSense{lexicalForm = otherLexForm}
              = lexForm == otherLexForm
            targetSenses -- actually there should only be one; if
                         -- there's more validation will catch it
              = NE.filter targetWord wordSenses
    findTarget _ = candidates

findSynset :: WNid -> ValIndex -> SynsetMap Validated -> Synset Validated
-- | find synset corresponding to human-readable id; after validation,
-- there should be exactly one
findSynset wnID index synsetMap =
  case lookupSenseCandidates wnID index synsetMap of
    [synset] -> synset
    _ -> error $ "Error trying to find synset of " ++ show wnID

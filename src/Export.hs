{-# LANGUAGE StrictData #-}
module Export where

import Data ( WNid(..), WNExtra(..), Relation(..), SynsetKey
            , Synset(..), Validated, WNPOS(..), RelationName
            , SynsetRelation(..), WSense(..), WordPointer(..)
            , LexicographerFileId(..), LexicalForm(..), synsetType
            , unsafeLookup, lexicographerFileIdToText
            , synsetKey, tshow, extraFrames, showLongWNPOS, senseKey
            )
import Validate (ValIndex, SynsetMap, lookupIndex
                , lookupSynset, findSynset, wordSenseKey, mapSynsets)
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder,charUtf8)
import Data.CaseInsensitive ( foldCase )
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.List (findIndex, mapAccumL, nub)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.ListTrie.Base.Map (WrappedIntMap)
import Data.ListTrie.Patricia.Map (TrieMap, lookupPrefix, foldlDescWithKey', fromListWith')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust -- oh, the pain
                  , catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)
import Prelude hiding (lookup)


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

identifyingRelations :: ValIndex -> SynsetMap Validated
  -> Map SynsetKey (Set (RelationName, LexicalForm))
identifyingRelations index synsetMap =
  M.fromListWith (<>) . concatMap go $ mapSynsets synsetMap
  where
    go Synset{wordSenses, relations} = catMaybes
      $ fmap (getIdRel . coerce) relations
      ++ concatMap (fmap getIdRel . coerce . pointers) wordSenses
    getIdRel (Relation _ WNid{idRel = Nothing}) = Nothing
    getIdRel (Relation _ wnid@WNid{idRel = Just (idRel, targetLexForm)})
      = Just (synsetKey targetSynset, S.singleton (idRel, targetLexForm))
      where
        targetSynset = findSynset wnid index synsetMap

---
-- JSON/aeson
synsetToJSON :: ValIndex -> SynsetMap Validated
  -> Map SynsetKey (Set (RelationName, LexicalForm))
  -> Map Text Text -> Map Text Int -> Synset Validated -> Value
synsetToJSON index synsetMap idRelsMap textToCanonicNames _
  synset@Synset{comments, wordSenses = wordSenses@(WSense{lexicalForm = headLexForm}:|_), ..}
  = object
    $ synsetFrames extra
    ++ idRels (idRelsMap M.!? synsetK)
    ++
    [ "id"         .= senseIdText headLexForm synsetK
    , "wordsenses" .= NE.map toWordSense wordSenses
    , "definition" .= definition
    , "examples"   .= examples
    , "relations"  .= map (toRelation . coerce) relations
    , "_position"   .= sourcePosition
    , "_comments"   .= comments
    ]
  where
    synsetK = synsetKey synset
    idRels Nothing = []
    idRels (Just xs) = [ "_references" .= map toIdRel (S.toList xs) ]
    toIdRel (name, targetLexForm) = (lookupRelName name, targetLexForm)
    senseIdText lexicalForm (wnName, wnPOS, lexname, offset) =
      T.intercalate "-" [wnName, showLongWNPOS wnPOS, lexname, coerce lexicalForm
                        , tshow offset]
    synsetFrames (WNVerb frames) = ["frames" .= NE.toList frames]
    synsetFrames _ = []
    toRelation (Relation name wnid@WNid{lexForm, idRel}) =
      object
      $ maybe [] (\x -> ["_reference" .= toIdRel x]) idRel
      ++ ["name" .= lookupRelName name
         , "id" .= senseIdText lexForm targetSynsetKey
         , "_targetLexicalForm" .= lexForm]
      where
        targetSynsetKey = synsetKey $ findSynset wnid index synsetMap
    lookupRelName name = unsafeLookup missingRelation name textToCanonicNames
      where
        missingRelation = "No relation with name " ++ show name ++ " found in relation.tsv"
    toWordSense (WSense lexicalForm wExtra pointers)
      = object $
        wordExtra wExtra
        ++ [ "lexicalForm" .= lexicalForm
           , "pointers" .= map (toRelation . coerce) pointers
           ]
      where
        wordExtra (WNAdj marker)  = ["syntacticMarker" .= marker]
        wordExtra (WNVerb frames) = ["frames" .= NE.toList frames]
        wordExtra _ = []


synsetsToSynsetJSONs :: ValIndex -> SynsetMap Validated -> Map Text Text -> Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs index synsetMap textToCanonicNames lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON index synsetMap idRelsMap textToCanonicNames lexNamesToLexNum) synsets
  where
    idRelsMap = identifyingRelations index synsetMap


---
-- WNDB

-- first we convert synsets to this structure which has all the info
-- we need
data DBSynset = DBSynset
  { _id                  :: SynsetKey
  , lexicographerFileNum :: Int
  , pos                  :: WNPOS
  , wordSenses           :: NonEmpty (Text, Int)
  , gloss                :: Text
  , frames               :: [(Int,Int)]
  , relations            :: [(Text, SynsetKey, WNPOS, (Int, Int))]
  } deriving (Show,Eq)

synsetLexFileNum :: Map Text Int -> Synset a -> Int
synsetLexFileNum lexMap Synset{lexicographerFileId}
  =
  let lexname = lexicographerFileIdToText lexicographerFileId
  in unsafeLookup
     ("Missing lexicographer name " ++ T.unpack lexname ++ " in lexnames.tsv")
     lexname
     lexMap

lexicalId :: ValIndex -> SynsetMap Validated -> Synset a -> WSense -> Int
lexicalId index synsetMap Synset{lexicographerFileId, sourcePosition} sense =
  fromJust $ findIndex sameSynset synsets
  where
    wordKey = wordSenseKey lexicographerFileId sense
    synsets = lookupIndex wordKey index synsetMap
    -- we use as lexicalId the index of synset under the key
    -- corresponding to the sense
    sameSynset Synset{sourcePosition = sourcePos, lexicographerFileId = lexFileId}
      = sourcePosition == sourcePos && lexFileId == lexicographerFileId

synsetToDB :: Map Text Text -> Map Text Int -> ValIndex -> SynsetMap Validated -> Synset Validated -> DBSynset
synsetToDB relationsMap lexicographerMap index synsetMap
  synset@Synset{lexicographerFileId, wordSenses = senses, definition, examples, extra, relations} =
  DBSynset { _id = synsetKey synset
           , lexicographerFileNum = synsetLexFileNum lexicographerMap synset
           , pos = pos (lexicographerFileId :: LexicographerFileId)
           , wordSenses = NE.map toWord senses -- [] FIXME: add syntactic marker
           , gloss = T.intercalate "; " (definition
                                         : map (\e -> T.cons '"' $ T.snoc e '"') examples)
           , frames = map synsetFrame (extraFrames extra)
                    ++ concatMap toWordFrames (zip [1..] $ NE.toList senses)
           , relations = map synsetRelation relations
                         ++ concatMap wordRelations (zip [1..] $ NE.toList senses)
           }
  where
    toWord sense@WSense{lexicalForm = LexicalForm form} =
      (form, lexicalId index synsetMap synset sense)
    synsetRelation (SynsetRelation (Relation relationName
                    wnIdentifier@WNid{pos=wnPoS}))
      = ( unsafeLookup ("Missing relation " ++ T.unpack relationName ++ " in relations.tsv") relationName relationsMap
        , synsetKey $ findSynset wnIdentifier index synsetMap, wnPoS, (0,0))
    synsetFrame = (0,)
    toWordFrames (ix, WSense{extra = wordExtra}) = map (ix,) $ extraFrames wordExtra
    wordRelations (ix, WSense _ _ wordPointers) =
      map (wordRelation ix) wordPointers
    wordRelation ix (WordPointer (Relation pointerName
                       wnId@WNid{pos=wnPoS})) =
      ( unsafeLookup ("Missing pointer " ++ T.unpack pointerName ++ "in relations.tsv") pointerName relationsMap
      , synsetKey targetSynset
      , wnPoS
      , (ix, getWordSenseIndex wnId targetSynset))
      where
        targetSynset = findSynset wnId index synsetMap
        getWordSenseIndex targetSenseId@WNid{lexForm = targetSenseForm} Synset{wordSenses} =
          fromMaybe
          (error $ "No wordsense corresponding to word sense " ++ show targetSenseId ++ " among " ++ show wordSenses)
          . findIndex (\WSense{lexicalForm} -> lexicalForm == targetSenseForm)
          $ NE.toList wordSenses

padText :: Int -> Text -> Text
padText n x = T.append (T.replicate (n - T.length x) "0") x

padNum :: Int -> Int -> Text
padNum n m = let x = T.pack $ show m in padText n x

wnIdentifierToOffset :: Map SynsetKey Int -> SynsetKey -> Text
wnIdentifierToOffset offsetMap synsetK =
  padNum 8 $ M.findWithDefault 0 synsetK offsetMap

showHumanPoS :: WNPOS -> Text
-- shows S as "a" instead of "s"
showHumanPoS N = "n"
showHumanPoS V = "v"
showHumanPoS S = "a"
showHumanPoS A = "a"
showHumanPoS R = "r"

-- render a DBSynset; this is done twice for each synset, the first
-- one has no offset data and is used to calculate it. in the second
-- run we can use the offsets. this is not optimized at all, but
-- performance seems acceptable, so who cares.
showDBSynset :: Map SynsetKey Int -> DBSynset -> Text
showDBSynset offsetMap DBSynset{ _id, lexicographerFileNum, pos, wordSenses, frames, relations, gloss}
  = T.unwords
  [ offsetDoc
  , padNum 2 lexicographerFileNum
  , showHumanPoS pos
  , wordCount
  , T.unwords . NE.toList $ NE.map synsetWord wordSenses
  , T.unwords $ pointerCount : map synsetPointer relations
  , if pos == V then T.unwords $ padNum 2 (length frames) : map synsetFrame frames else ""
  , "|", gloss]
  where
    pointerCount = padNum 3 $ length relations
    wordCount = paddedHex 2 $ NE.length wordSenses
    idOffset = wnIdentifierToOffset offsetMap . coerce
    offsetDoc = idOffset _id 
    paddedHex n h = padText n . T.pack $ showHex h ""
    synsetWord (wordForm, lexId) = T.unwords [T.replace " " "_" wordForm, T.pack $ showHex lexId ""]
    synsetPointer (pointerSym, targetId, targetPos, (sourceNum,targetNum))
      = T.unwords [ pointerSym, idOffset targetId, showHumanPoS targetPos
                  , paddedHex 2 sourceNum <> paddedHex 2 targetNum ]
    synsetFrame (frameNum, wordNum) = padNum 2 frameNum <> " " <> paddedHex 2 wordNum

newline :: Text
newline = "\n"

calculateOffsets :: Int -> Map SynsetKey Int -> Map Text Text -> Map Text Int -> ValIndex -> SynsetMap Validated -> NonEmpty (Synset Validated)
  -> (Map SynsetKey Int, NonEmpty DBSynset)
calculateOffsets startOffset startOffsetMap relationsMap lexicographerMap index synsetMap synsets
  = (\((offsetMap, _), dbSynsets) -> (offsetMap, dbSynsets)) $ mapAccumL go (startOffsetMap, startOffset) synsets
  where
    getBytesize = B.length . encodeUtf8
    newlineSize = getBytesize newline
    go (offsetMap, offset) synset =
      let dbSynset@DBSynset{_id} = synsetToDB relationsMap lexicographerMap index synsetMap synset
          increment = getBytesize $ showDBSynset M.empty dbSynset
      in ( (M.insert _id offset offsetMap, offset + increment + newlineSize)
         , dbSynset
         )

-- same as ValIndex but keys are case-insensitive
type WNDBindex = TrieMap WrappedIntMap Char (Set SynsetKey)

-- relates to wndbindexlemmakey
wndbIndex :: SynsetMap a -> WNDBindex
-- make index for index* files (which see
-- https://wordnet.princeton.edu/documentation/wndb5wn) contain case
-- insensitive lemmas
wndbIndex synsetMap = fromListWith' (<>) pairs
  where
    pairs = concatMap go $ M.assocs synsetMap
    go (key, Synset{lexicographerFileId = LexicographerFileId{pos}, wordSenses})
      = NE.toList $ NE.map (posPair key) wordSenses
      where
        posPair synsetK WSense{lexicalForm = LexicalForm lexForm}
          = (concat [show (if pos == S then A else pos), "\t", foldCase $ T.unpack lexForm], S.singleton synsetK)

wndbIndexLemmaKey :: String -> Text
wndbIndexLemmaKey = T.pack . drop 1 . dropWhile (/= '\t')

--- lemma  pos  synset_cnt  p_cnt  [ptr_symbol...]  sense_cnt  tagsense_cnt   synset_offset  [synset_offset...]
showWNDBindex :: WNPOS -> Map Text Text -> Map SynsetKey Int -> WNDBindex -> SynsetMap a -> [Text]
showWNDBindex wnPos relationsLexName offsetMap wNDBindex synsetMap = foldlDescWithKey' go [] posIndex
  where
    go key synsetKeysSet resultSoFar = line key (S.toList synsetKeysSet) : resultSoFar
    line key synsetKeys =
        T.unwords [ lemma
                  , pos
                  , synsetCount
                  , T.unwords $ tshow (length lemmaRelations) : map toLexRelationName lemmaRelations
                  , synsetCount
                  , "0" -- FIXME: count of tags of sense in corpus (not implemented)
                  , T.unwords $ fmap synsetOffset synsets
                  ]
      where
        -- see wndbIndex, but key is pos\tlemma
        lemma = wndbIndexLemmaKey key
        synsetCount = tshow $ length synsetKeys
        synsets = (synsetMap `lookupSynset`) <$> synsetKeys
        lemmaRelations = nub $ concatMap (wordRelations lemma) synsets
    wordRelations lemma Synset{relations, wordSenses} = concatMap lemmaPointers wordSenses ++ map (relationName . coerce) relations
      where
        lemmaPointers WSense{lexicalForm=LexicalForm wordForm, pointers}
          = if foldCase wordForm == lemma then map (relationName . coerce) pointers else []
    relationName (Relation name _) = name
    toLexRelationName relName = unsafeLookup ("Missing relation " ++ T.unpack relName ++ " in relations.tsv") relName relationsLexName
    synsetOffset = wnIdentifierToOffset offsetMap . synsetKey
    posIndex = lookupPrefix (T.unpack $ T.toUpper pos) wNDBindex
    pos = showHumanPoS wnPos

wndbSenseIndex :: Map Text Int -> ValIndex -> SynsetMap Validated
  -> WNDBindex -> Map SynsetKey Int -> [Text]
wndbSenseIndex lexicographerMap index synsetMap wNDBIndex offsetMap
  = foldlDescWithKey' go [] wNDBIndex
  where
    go key synsetKeysSet resultSoFar =
      -- fake or not, sense number starts from 1
      zipWith (line key) synsetKeys ([1..] :: [Int])
      ++ resultSoFar
      where
        synsetKeys = S.toList synsetKeysSet
    -- hardcoded alert
    getHeadId (SynsetRelation (Relation "sim" wnid)) = Just wnid
    getHeadId _ = Nothing
    line key synsetK n =
      T.unwords [ T.pack sensekey -- sensekey
                , wnIdentifierToOffset offsetMap synsetK -- offset
                , tshow n -- sense number (faked)
                , "0" -- FIXME: count tags of sense in corpus
                ]
      where
        sensekey = senseKey lexFileNum synsetTypeNum lexId maybeHead sense
        lexFileNum = synsetLexFileNum lexicographerMap synset
        synsetTypeNum = synsetType synsetPOS
        synsetPOS = (pos :: LexicographerFileId -> WNPOS) $ lexicographerFileId synset
        lexId = lexicalId index synsetMap synset sense
        lemma = wndbIndexLemmaKey key
        synset = synsetMap `lookupSynset` synsetK
        sense = findSense lemma synset
        findSense senseLemma lemmaSynset = fromJust
          . find (\WSense{lexicalForm} -> foldCase (coerce lexicalForm) == senseLemma)
          $ Data.wordSenses lemmaSynset
        maybeHead = case synsetPOS of
          S -> case mapMaybe getHeadId $ Data.relations synset of
            [wnid@WNid{lexForm = LexicalForm lexForm}]
              -> let headSynset = findSynset wnid index synsetMap
                     headSense = findSense lexForm headSynset
                 in Just ( LexicalForm lexForm
                         , lexicalId index synsetMap headSynset headSense)
            _ -> error "No head or more than one head"
          _ -> Nothing

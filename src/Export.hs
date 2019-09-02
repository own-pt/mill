{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Export where

import Data ( WordSenseForm(..), LexicographerFileId(..)
            , LexicalId(..), WordSenseIdentifier(..)
            , SynsetIdentifier(..), Synset(..), Validated
            , SynsetRelation(..), WordPointer(..), WNWord(..)
            , lexicographerFileIdToText, senseKey, synsetType
            , WNPOS(..) )
---
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (ToJSON(..), genericToEncoding, defaultOptions, fromEncoding)
import Data.ByteString.Builder (Builder,charUtf8)
import qualified Data.DList as DL
import Data.List (find)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.RDF.ToRDF (ToObject(..), RDFGen, appBaseIRI,Triples)
import Data.RDF.Types ( Subject(..), Predicate(..), Object(..)
                      , IRI(..), Triple(..) )
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

---
-- RDF
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

synsetToTriples :: Map Text Text -> Map Text Text -> Synset Validated -> RDFGen Triples
synsetToTriples textToCanonicMap canonicToRDFMap
  Synset{lexicographerFileId, wordSenses, definition, examples
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
    lookupRelation relationName textToCanonicMap' canonicToRDFMap'
      =  case M.lookup relationName textToCanonicMap' of
           Nothing -> fromMaybe (error $ "Inexisting relation "
                                          ++ T.unpack relationName ++ " on relations.tsv")
                         $ M.lookup relationName canonicToRDFMap'
           Just canonicName -> fromMaybe (error $ "Inexisting relation "
                                          ++ T.unpack relationName ++ " on relations.tsv")
                         $ M.lookup canonicName canonicToRDFMap'
    toRDFName textName =
      lookupRelation textName textToCanonicMap canonicToRDFMap
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

---
-- JSON/aeson
data SynsetJSON =
  SynsetJSON { lexname    :: Text
             , pos        :: Char
             , keys       :: NonEmpty String
             , terms      :: NonEmpty WordSenseForm
             , definition :: Text
             , examples   :: [Text]
             } deriving (Generic,Show)

instance ToJSON SynsetJSON where
    toEncoding = genericToEncoding defaultOptions

synsetToSynsetJSON :: Map Text Int -> Synset Validated -> SynsetJSON
synsetToSynsetJSON lexNamesToLexNum
  Synset{lexicographerFileId = lexFileId@(LexicographerFileId (wnPOS, lexName'))
        , wordSenses, definition, examples, relations
        }
  = SynsetJSON { lexname    = lexName'
               , pos        = head $ show wnPOS
               , terms      = NE.map toTerm wordSenses
               , keys       =
                 NE.map (senseKey lexFileNum (synsetType wnPOS) maybeHeadSynset) wordSenses
               , definition = definition
               , examples   = examples
               }
  where
    lexName  = lexicographerFileIdToText lexFileId
    lexFileNum = fromMaybe (error $ "No lexfile with name "
                            ++ show lexName ++ "found in lexnames.tsv")
                 $ M.lookup lexName lexNamesToLexNum
    toTerm (WNWord (WordSenseIdentifier (_,wordForm,_)) _ _) = wordForm
    isHeadRelation (SynsetRelation "sim" _) = True
    isHeadRelation _ = False
    maybeHeadSynset = case wnPOS of
      S -> find isHeadRelation relations
      _ -> Nothing

synsetsToSynsetJSONs :: Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToSynsetJSON lexNamesToLexNum) synsets

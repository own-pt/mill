module Export where

import Data ( LexicographerFileId(..)
            , WordSenseIdentifier(..), Synset(..), Validated
            , SynsetRelation(..), WNWord(..), WordPointer(..)
            , lexicographerFileIdToText, senseKey, synsetType
            , WNPOS(..), unsafeLookup )
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import Data.ByteString.Builder (Builder,charUtf8)
import Data.List (find)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Text (Text)

---
-- JSON/aeson
synsetToJSON :: Map Text Text -> Map Text Int -> Synset Validated -> Value
synsetToJSON textToCanonicNames lexNamesToLexNum
  Synset{wordSenses = wordSenses@(WNWord headWordId@(WordSenseIdentifier (lexFileId@(LexicographerFileId (wnPOS, _)), _, _)) _ _:|_), ..}
  = object
  [ "id"         .= headWordId
  , "wordsenses" .= NE.map toWordSense wordSenses
  , "definition" .= definition
  , "examples"   .= examples
  , "frames"     .= frames
  , "relations"  .= map (\(SynsetRelation name targetIdentifier) -> toRelation name targetIdentifier) relations
  , "position"   .= sourcePosition
  ]
  where
    toRelation name wnIdentifier = object ["name" .= unsafeLookup (missingRelation name) name textToCanonicNames, "id" .= wnIdentifier]
    missingRelation name = "No relation with name " ++ show name ++ " found in relation.tsv"
    toWordSense wordSense@(WNWord (WordSenseIdentifier (_, lexForm, lexId)) wordFrames pointers)
      = object
      [ "lexicalForm" .= lexForm
      , "lexicalId" .= lexId
      , "frames" .= wordFrames
      , "pointers" .= map (\(WordPointer name targetIdentifier) -> toRelation name targetIdentifier) pointers
      , "senseKey" .= senseKey lexFileNum (synsetType wnPOS) maybeHeadSynset wordSense
      ]
    lexfileName  = lexicographerFileIdToText lexFileId
    lexFileNum = unsafeLookup ("No lexfile with name "
                               ++ show lexfileName ++ "found in lexnames.tsv")
                 lexfileName lexNamesToLexNum
    isHeadRelation (SynsetRelation "sim" _) = True
    isHeadRelation _ = False
    maybeHeadSynset = case wnPOS of
      S -> find isHeadRelation relations
      _ -> Nothing

synsetsToSynsetJSONs :: Map Text Text -> Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs textToCanonicNames lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON textToCanonicNames lexNamesToLexNum) synsets

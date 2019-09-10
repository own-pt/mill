module Export where

import Data ( LexicographerFileId(..)
            , WordSenseIdentifier(..), Synset(..), Validated
            , SynsetRelation(..), WNWord(..)
            , lexicographerFileIdToText, senseKey, synsetType
            , WNPOS(..) )
---
import Data.Aeson ( ToJSON(..), fromEncoding, Value
                  , object, (.=) )
import Data.ByteString.Builder (Builder,charUtf8)
import Data.Char (toLower)
import Data.List (find)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)

---
-- JSON/aeson
synsetToJSON :: Map Text Int -> Synset Validated -> Value
synsetToJSON lexNamesToLexNum
  Synset{wordSenses = wordSenses@(WNWord (WordSenseIdentifier (lexFileId@(LexicographerFileId (wnPOS, lexname)), headWordForm, headLexId)) _ _:|_), ..}
  = object
  [ "id"         .= (toLower . head $ show wnPOS, lexname, headWordForm, headLexId)
  , "wordsenses" .= NE.map toWordSense wordSenses
  , "definition" .= definition
  , "examples"   .= examples
  , "frames"     .= frames
  , "relations"  .= relations
  , "position"   .= sourcePosition
  ]
  where
    toWordSense wordSense@(WNWord (WordSenseIdentifier (_,wordForm,lexId)) wordFrames pointers)
      = (wordForm, lexId, wordFrames, pointers, senseKey lexFileNum (synsetType wnPOS) maybeHeadSynset wordSense)
    lexfileName  = lexicographerFileIdToText lexFileId
    lexFileNum = fromMaybe (error $ "No lexfile with name "
                            ++ show lexfileName ++ "found in lexnames.tsv")
                 $ M.lookup lexfileName lexNamesToLexNum
    isHeadRelation (SynsetRelation "sim" _) = True
    isHeadRelation _ = False
    maybeHeadSynset = case wnPOS of
      S -> find isHeadRelation relations
      _ -> Nothing

synsetsToSynsetJSONs :: Map Text Int -> NonEmpty (Synset Validated) -> Builder
synsetsToSynsetJSONs lexNamesToLexNum synsets
  = mconcat . NE.toList . NE.intersperse (charUtf8 '\n')
  $ NE.map (fromEncoding . toEncoding . synsetToJSON lexNamesToLexNum) synsets

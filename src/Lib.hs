module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    , validateLexicographerFiles
    , lexicographerFilesToTriples
    , readConfig
    , lexicographerFilesInDirectoryToTriples
    ) where

import Data ( Synset(..), Unvalidated, Validated
            , synsetToTriples, Validation(..), SourceValidation )
import Parse (parseLexicographer)
import Validate ( makeIndex
                , validateSynsets )
----------------------------------
import Control.Monad (unless,(>>))
import Control.Monad.Reader (ReaderT(..), ask, liftIO)
import Data.Binary (encodeFile)
import Data.Binary.Builder (toLazyByteString)
import qualified Data.DList as DL
import Data.Either (partitionEithers)
import Data.Functor(void)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RDF.Encoder.NQuads (encodeRDFGraph)
import Data.RDF.ToRDF (runRDFGen)
import Data.RDF.Types (RDFGraph(..), IRI(..))
import Data.Semigroup (sconcat)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath ((</>), normalise,equalFilePath)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

-- | This datastructure contains the information found in the
-- configuration files (currently only lexnames.tsv and relations.tsv
-- are considered.)
data Config = Config
  { -- | maps lexicographer filenames to their numerical ids
    lexnamesToId       :: Map Text Int
  -- | maps relation names in text files to their canonical names
  , textToCanonicNames :: Map Text Text
  -- | maps canonical relation names to their RDF names
  , canonicToRDFNames  :: Map Text Text
  -- | contains the filepaths to the files in the WordNet
  , lexFilePaths       :: NonEmpty FilePath
  } deriving (Show,Eq)

readTSV :: Ord a => FilePath -> ([Text] -> Either String [(a, b)]) -> IO (Map a b)
readTSV filepath readLine = do
  text <- TIO.readFile filepath
  case go text of
    ([], pairs) -> return . M.fromList $ concat pairs
    (errors, _) -> mapM_ putStrLn errors >> return M.empty
  where
    go = partitionEithers . map readLine
      . filter (not . isComment)
      . map (map T.strip . T.splitOn "\t") . drop 1 . T.lines
    isComment [field] = T.take 2 field == "--"
    isComment _ = False

readConfig :: FilePath -> IO Config
readConfig configurationDir' = do
  configurationDir   <- canonicalizePath configurationDir'
  isDirectory        <- doesDirectoryExist configurationDir
  unless isDirectory (error "Filepath must be a directory")
  go configurationDir
  where
    go configurationDir = do
      lexnamesToId       <- readTSV (configurationDir </> "lexnames.tsv") lexnamesReader
      -- FIXME: reading file twice, but oh well..
      textToCanonicNames <- readTSV (configurationDir </> "relations.tsv") textToCanonicNamesReader
      canonicToRDFNames  <- readTSV (configurationDir </> "relations.tsv") canonicToRDFNamesReader
      let lexFilePaths = lexFilePaths' lexnamesToId
      return $ Config {lexnamesToId, textToCanonicNames, canonicToRDFNames, lexFilePaths}
        where
          lexFilePaths' lexNamesMap =
            let lexnames = map normalize (M.keys lexNamesMap)
            in if null lexnames
            then error "No files specified in lexnames.tsv"
            else NE.fromList lexnames
          normalize lexFileId = configurationDir </> T.unpack lexFileId
          lexnamesReader [lexnameIdStr, lexicographerFile, _] =
            case TR.decimal lexnameIdStr of
              Left err -> Left err
              Right (lexnameId, "") -> Right [(lexicographerFile, lexnameId)]
              Right (_, trailing) -> Left $ "Trailing garbage after " ++ T.unpack trailing
          lexnamesReader _ = Left "Wrong number of fields in lexnames.tsv"
          textToCanonicNamesReader [_,_,"_",_,_,_,_]      = Right []
          textToCanonicNamesReader [canonicName,_,textName,_,_,_,_] = Right [(textName, canonicName)]
          textToCanonicNamesReader _ = Left "Wrong number of fields in relations.tsv"
          canonicToRDFNamesReader  [canonicName,_,_,rdfName,_,_,_] = Right [(canonicName, rdfName)]
          canonicToRDFNamesReader _ = Left "Wrong number of fields in relations.tsv"

type App = ReaderT Config IO

parseLexicographerFile :: FilePath -> App (SourceValidation (NonEmpty (Synset Unvalidated)))
parseLexicographerFile filePath = do
  Config{textToCanonicNames} <- ask
  liftIO $ do
    content <- TIO.readFile $ normalise filePath
    let result = parseLexicographer textToCanonicNames filePath content
    return result

parseLexicographerFiles ::  NonEmpty FilePath
  -> App (SourceValidation (NonEmpty (Synset Validated)))
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile filePaths
  case sequenceA lexFilesSynsetsOrErrors of
    Success lexFilesSynsets ->
      let synsets = sconcat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsets index synsets
    Failure sourceErrors -> return $ Failure sourceErrors

prettyPrintList :: Pretty a => NonEmpty a -> IO ()
prettyPrintList = mapM_ (putDoc . pretty)

validateSynsetsNoParseErrors :: NonEmpty (NonEmpty (Synset Unvalidated))
  -> Maybe (NonEmpty (Synset Unvalidated))
  -> IO ()
validateSynsetsNoParseErrors indexSynsets maybeSynsetsToValidate =
  let synsets = sconcat indexSynsets
      index   = makeIndex synsets
  in case validateSynsets index (fromMaybe synsets maybeSynsetsToValidate) of
    Success _ -> return ()
    Failure errors -> prettyPrintList errors
    
validateLexicographerFile :: FilePath -> App ()
validateLexicographerFile filePath = do
  let normalFilePath = normalise filePath
  Config{lexFilePaths} <- ask
  case partition (equalFilePath normalFilePath) $ NE.toList lexFilePaths of
    ([fileToValidate], otherLexicographerFiles) -> go fileToValidate otherLexicographerFiles
    ([], lexFiles) -> go normalFilePath lexFiles
    _       -> liftIO . putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go fileToValidate otherFiles = do
      lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:|otherFiles)
      liftIO $ case sequenceA lexFilesSynsetsOrErrors of
        Failure parseErrors
          -> prettyPrintList parseErrors
        Success lexFilesSynsets@(synsetsToValidate:|_)
          -> validateSynsetsNoParseErrors lexFilesSynsets (Just synsetsToValidate)

validateLexicographerFiles :: App ()
validateLexicographerFiles = do
  Config{lexFilePaths} <- ask
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile lexFilePaths
  liftIO $ case sequenceA lexFilesSynsetsOrErrors of
    Failure parseErrors
      -> prettyPrintList parseErrors
    Success lexFilesSynsets
      -> validateSynsetsNoParseErrors lexFilesSynsets Nothing

synsetsToTriples :: Map Text Text -> Map Text Text
  -> IRI -> NonEmpty (Synset Validated) -> FilePath -> IO ()
synsetsToTriples textToCanonicMap canonicToRDFMap baseIRI synsets outputFile =
  encodeFile outputFile
  . toLazyByteString
  . encodeRDFGraph . RDFGraph Nothing $ DL.toList synsetsTriples
  where
    synsetsTriples = foldMap (\synset -> runRDFGen
                               (synsetToTriples textToCanonicMap canonicToRDFMap synset)
                               baseIRI)
                     synsets

lexicographerFilesToTriples :: IRI -> FilePath -> App ()
lexicographerFilesToTriples baseIRI outputFile = do
  Config{textToCanonicNames, canonicToRDFNames, lexFilePaths} <- ask
  synsetsValid <- parseLexicographerFiles lexFilePaths
  liftIO $ case synsetsValid of
    (Success synsets) -> synsetsToTriples textToCanonicNames
                                          canonicToRDFNames baseIRI synsets outputFile
    (Failure _) -> void
      $ putStrLn "Errors in lexicographer files, please validate them before exporting."

lexicographerFilesInDirectoryToTriples :: String -> FilePath -> App ()
lexicographerFilesInDirectoryToTriples baseIriString =
  lexicographerFilesToTriples (fromString baseIriString)
  

module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    , validateLexicographerFiles
    , lexicographerFilesToTriples
    , readConfig
    , lexicographerFilesInDirectoryToTriples
    ) where

import Data (Synset,Unvalidated,Validated)
import Parse (parseLexicographer)
import Validate ( Validation(..), makeIndex
                , validateSynsets, SourceValidation)
----------------------------------
import Control.Monad (unless,(>>))
import Data.Binary (encodeFile)
import Data.Binary.Builder (toLazyByteString)
import Data.Either (partitionEithers)
import Data.Functor(void)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RDF.Encoder.NQuads (encodeRDFGraph)
import Data.RDF.ToRDF (toTriples)
import Data.RDF.Types (RDFGraph(..), IRI(..))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), takeDirectory,normalise,equalFilePath)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)


parseLexicographerFile :: FilePath -> IO (SourceValidation (NonEmpty (Synset Unvalidated)))
parseLexicographerFile filePath = do
  content <- TIO.readFile $ normalise filePath
  let result = parseLexicographer filePath content
  return result

parseLexicographerFiles :: NonEmpty FilePath
  -> IO (SourceValidation (NonEmpty (Synset Validated)))
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile filePaths
  case sequenceA lexFilesSynsetsOrErrors of
    Success lexFilesSynsets ->
      let synsets = sconcat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsets index synsets
    Failure sourceErrors -> return $ Failure sourceErrors

data Config = Config
  { lexnamesToId :: Map Text Int
  , relationRDFNames :: Map Text Text
  } deriving (Show,Eq)

readTSV :: Ord a => FilePath -> ([Text] -> Either String (a, b)) -> IO (Map a b)
readTSV filepath readLine = do
  text <- TIO.readFile filepath
  case go text of
    ([], pairs) -> return $ M.fromList pairs
    (errors, _) -> mapM_ putStrLn errors >> return M.empty
  where
    go = partitionEithers . map readLine
      . filter (not . isComment)
      . map (T.splitOn "\t") . drop 1 . T.lines
    isComment [field] = let field' = T.strip field in T.take 2 field' == "--"
    isComment _ = False

readConfig :: FilePath -> IO Config
readConfig configurationDir = do
  isDirectory <- doesDirectoryExist configurationDir
  unless isDirectory (putStrLn "Filepath must be a directory")
  lexnamesToId' <- readTSV (configurationDir </> "lexnames.tsv") lexnamesReader
  relationRDFNames' <- readTSV (configurationDir </> "relations.tsv") relationsReader
  return $ Config lexnamesToId' relationRDFNames'
  where
    lexnamesReader [lexnameIdStr, lexicographerFile, _] =
      case TR.decimal lexnameIdStr of
        Left err -> Left err
        Right (lexnameId, "") -> Right (lexicographerFile, lexnameId)
        Right (_, trailing) -> Left $ "Trailing garbage after " ++ T.unpack trailing
    lexnamesReader _ = Left "Wrong number of fields in lexnames.tsv"
    relationsReader [_,relationName,rdfName,_,_,_] = Right (relationName, rdfName)
    relationsReader _ = Left "Wrong number of fields in relations.tsv"

lexicographerFilesInDirectory :: FilePath -> IO (NonEmpty FilePath)
lexicographerFilesInDirectory filesDirectory = do
  doesDirectoryExist' <- doesDirectoryExist filesDirectory
  if doesDirectoryExist'
    then do
      Config{lexnamesToId} <- readConfig filesDirectory
      let lexnames = map (normalise . go . fst)
                       $ M.toList lexnamesToId
      if null lexnames
        then error "No files specified in lexnames.tsv"
        else return $ NE.fromList lexnames
    else
    error ("Directory " ++ filesDirectory ++ "does not exist.")
  where
    go lexFileId = filesDirectory </> T.unpack lexFileId


prettyPrintList :: Pretty a => NonEmpty a -> IO ()
prettyPrintList = mapM_ (putDoc . pretty)

validateSynsetsNoParseErrors :: NonEmpty (NonEmpty (Synset Unvalidated)) -> Maybe (NonEmpty (Synset Unvalidated)) -> IO ()
validateSynsetsNoParseErrors indexSynsets maybeSynsetsToValidate =
  let synsets = sconcat indexSynsets
      index   = makeIndex synsets
  in case validateSynsets index (fromMaybe synsets maybeSynsetsToValidate) of
    Success _ -> return ()
    Failure errors -> prettyPrintList errors
    
validateLexicographerFile :: FilePath -> IO ()
validateLexicographerFile filePath = do
  let normalFilePath = normalise filePath
  lexicographerFiles <- lexicographerFilesInDirectory $ takeDirectory normalFilePath
  case partition (equalFilePath normalFilePath) $ NE.toList lexicographerFiles of
    ([fileToValidate], otherLexicographerFiles) -> go fileToValidate otherLexicographerFiles
    ([], lexFiles) -> go normalFilePath lexFiles
    _       -> putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go fileToValidate otherFiles = do
      lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:|otherFiles)
      case sequenceA lexFilesSynsetsOrErrors of
        Failure parseErrors
          -> prettyPrintList parseErrors
        Success lexFilesSynsets@(synsetsToValidate:|_)
          -> validateSynsetsNoParseErrors lexFilesSynsets (Just synsetsToValidate)

validateLexicographerFiles :: FilePath -> IO ()
validateLexicographerFiles filesDirectory = do
  lexicographerFiles <- lexicographerFilesInDirectory filesDirectory
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile lexicographerFiles
  case sequenceA lexFilesSynsetsOrErrors of
    Failure parseErrors
      -> prettyPrintList parseErrors
    Success lexFilesSynsets
      -> validateSynsetsNoParseErrors lexFilesSynsets Nothing

wn30 :: IRI
wn30 = "https://w3id.org/own-pt/wn30-en/"

synsetsToTriples :: NonEmpty (Synset Validated) -> FilePath -> IO ()
synsetsToTriples synsets outputFile =
  encodeFile outputFile
  . toLazyByteString
  . encodeRDFGraph $ RDFGraph Nothing synsetsTriples
  where
    synsetsTriples = concatMap (toTriples wn30) synsets

lexicographerFilesToTriples :: NonEmpty FilePath -> FilePath -> IO ()
lexicographerFilesToTriples fileNames outputFile = do
  synsetsValid <- parseLexicographerFiles fileNames
  case synsetsValid of
    (Success synsets) -> synsetsToTriples synsets outputFile
    (Failure _) -> void
      $ putStrLn "Errors in lexicographer files, please validate them before exporting."

lexicographerFilesInDirectoryToTriples :: FilePath -> FilePath -> IO ()
lexicographerFilesInDirectoryToTriples lexicographerDir outputFile = do
  lexicographerFiles <- lexicographerFilesInDirectory lexicographerDir
  lexicographerFilesToTriples lexicographerFiles outputFile
  

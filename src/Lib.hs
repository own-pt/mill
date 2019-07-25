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
import Validate ( validateSynsetsInIndex, Validation(..), makeIndex
                , validateSynsets, syntaxSourceErrors
                , SourceValidation)
----------------------------------
import Control.Monad (unless,(>>))
import Data.Binary (encodeFile)
import Data.Binary.Builder (toLazyByteString)
import Data.Either
import Data.Functor(($>),void)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RDF.Encoder.NQuads (encodeRDFGraph)
import Data.RDF.ToRDF (toTriples)
import Data.RDF.Types (RDFGraph(..), IRI(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>), takeDirectory,normalise,equalFilePath)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)


parseLexicographerFile :: FilePath -> IO (Either () [Synset Unvalidated])
parseLexicographerFile filePath = do
  content <- TIO.readFile $ normalise filePath
  case parseLexicographer filePath content of
    Left err -> putStr err $> Left ()
    Right lexFileSynsets ->
      return $ Right lexFileSynsets

parseLexicographerFiles :: [FilePath] -> IO (SourceValidation [Synset Validated])
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile filePaths
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsetsInIndex index
    _ -> return $ Failure syntaxSourceErrors

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

lexicographerFilesInDirectory :: FilePath -> IO [FilePath]
lexicographerFilesInDirectory filesDirectory = do
  doesDirectoryExist' <- doesDirectoryExist filesDirectory
  if doesDirectoryExist'
    then do
      Config{lexnamesToId} <- readConfig filesDirectory
      let lexnames = map (normalise . go . fst)
                       $ M.toList lexnamesToId
      return lexnames
    else
    putStrLn ("Directory " ++ filesDirectory ++ "does not exist.") >> return []
  where
    go lexFileId = filesDirectory </> T.unpack lexFileId

printValidation :: SourceValidation [Synset Validated] -> IO ()
printValidation (Failure errors) = mapM_ (putDoc . pretty) errors
printValidation _ = return ()
    
validateLexicographerFile :: FilePath -> IO ()
validateLexicographerFile filePath = do
  let normalFilePath = normalise filePath
  lexicographerFiles <- lexicographerFilesInDirectory $ takeDirectory normalFilePath
  case partition (equalFilePath normalFilePath) lexicographerFiles of
    ([fileToValidate], otherLexicographerFiles) -> go fileToValidate otherLexicographerFiles
    ([], lexFiles) -> go normalFilePath lexFiles
    _       -> putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go fileToValidate otherFiles = do
      lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:otherFiles)
      case partitionEithers lexFilesSynsetsOrErrors of
        ([], lexFilesSynsets@(synsetsToValidate:_)) ->
          let synsets = concat lexFilesSynsets
              index   = makeIndex synsets
          in printValidation $ validateSynsets index synsetsToValidate
        _ -> return ()

validateLexicographerFiles :: FilePath -> IO ()
validateLexicographerFiles filesDirectory = do
  lexicographerFiles <- lexicographerFilesInDirectory filesDirectory
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile lexicographerFiles
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexfilesSynsets) ->
      let synsets = concat lexfilesSynsets
          index = makeIndex synsets
          in printValidation $ validateSynsets index synsets
    _ -> return ()


wn30 :: IRI
wn30 = "https://w3id.org/own-pt/wn30-en/"

synsetsToTriples :: [Synset Validated] -> FilePath -> IO ()
synsetsToTriples synsets outputFile =
  encodeFile outputFile
  . toLazyByteString
  . encodeRDFGraph $ RDFGraph Nothing synsetsTriples
  where
    synsetsTriples = concatMap (toTriples wn30) synsets

lexicographerFilesToTriples :: [FilePath] -> FilePath -> IO ()
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
  

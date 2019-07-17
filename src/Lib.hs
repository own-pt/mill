module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    , validateLexicographerFiles
    , lexicographerFilesToTriples
    , readConfig
    ) where

import Data (Synset,Unvalidated,Validated)
import Parse (parseLexicographer)
import Validate (validateSynsetsInIndex, Validation(..), SourceError, makeIndex
                , validateSynsets, validation, showSourceError)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (unless,(>>))
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Functor(($>),void)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesDirectoryExist)
import Data.RDF.Encoder.NQuads (encodeRDFGraph)
import Data.RDF.ToRDF (toTriples)
import Data.RDF.Types (RDFGraph(..), IRI(..))
import Data.Binary (encodeFile)
import Data.Binary.Builder (toLazyByteString)


parseLexicographerFile :: FilePath -> IO (Either () [Synset Unvalidated])
parseLexicographerFile fileName = do
  content <- TIO.readFile fileName
  case parseLexicographer fileName content of
    Left err -> putStr err $> Left ()
    Right (_, _, lexFileSynsets) ->
      return $ Right lexFileSynsets

parseLexicographerFiles :: [FilePath] -> IO (Validation [SourceError] [Synset Validated])
parseLexicographerFiles fileNames = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile fileNames
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsetsInIndex index
    _ -> return (Failure [])

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
      let lexnames = map ((</>) filesDirectory . T.unpack . fst)
                       $ M.toList lexnamesToId
      return lexnames
    else
    putStrLn ("Directory " ++ filesDirectory ++ "does not exist.") >> return []


validateLexicographerFile :: FilePath -> IO ()
validateLexicographerFile fileName = do
  lexicographerFiles <- lexicographerFilesInDirectory $ takeDirectory fileName
  let otherLexicographerFiles = filter (/= fileName) lexicographerFiles
  go fileName otherLexicographerFiles
  where
    go fileToValidate otherFiles = do
      lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:otherFiles)
      case partitionEithers lexFilesSynsetsOrErrors of
        ([], lexFilesSynsets@(synsetsToValidate:_)) ->
          let synsets = concat lexFilesSynsets
              index   = makeIndex synsets
          in validation (mapM_ (TIO.putStrLn . showSourceError)) (void . return)
             $ validateSynsets index synsetsToValidate
        _ -> return ()

validateLexicographerFiles :: FilePath -> IO ()
validateLexicographerFiles filesDirectory = do
  lexicographerFiles <- lexicographerFilesInDirectory filesDirectory
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile lexicographerFiles
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexfilesSynsets) ->
      let synsets = concat lexfilesSynsets
          index = makeIndex synsets
          in validation (mapM_ (TIO.putStrLn . showSourceError)) (void . return)
               $ validateSynsets index synsets
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
    (Failure _) -> void $ putStrLn "error"

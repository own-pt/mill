module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    ) where

import Data (Synset,Unvalidated,Validated)
import Parse (parseLexicographer)
import Validate

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad (unless,(>>))
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Functor(($>),void)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)


parseLexicographerFile :: FilePath -> IO (Either () [Synset Unvalidated])
parseLexicographerFile fileName = do
  content <- TIO.readFile fileName
  case parseLexicographer fileName content of
    Left err -> putStr err $> Left ()
    Right (_, _, lexFileSynsets) -> 
      return $ Right lexFileSynsets

parseLexicographerFiles :: [FilePath] -> IO (Validation [SourceError] (Index (Synset Validated)))
parseLexicographerFiles fileNames = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile fileNames
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsetsInIndex index
    _ -> return (Failure [])

validateLexicographerFile :: FilePath -> [FilePath] -> IO ()
validateLexicographerFile fileToValidate otherFiles = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:otherFiles)
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets@(synsetsToValidate:_)) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in validation (mapM_ (TIO.putStrLn . showSourceError)) (void . return) $ validateSynsets index synsetsToValidate
    _ -> return ()

data Config = Config
  { lexnames :: Map Text Int
  } deriving (Show,Eq)

readTSV :: Ord a => FilePath -> ([Text] -> Either String (a, b)) -> IO (Map a b)
readTSV filepath readLine = do
  text <- TIO.readFile filepath
  case go text of
    ([], pairs) -> return $ M.fromList pairs
    (errors, _) -> mapM_ putStrLn errors >> return M.empty
  where
    go = partitionEithers . map readLine . filter (not . isComment) . map (T.splitOn "\t") . drop 1 . T.lines
    isComment [field] = let field' = T.strip field in T.take 2 field' == "--"
    isComment _ = False

readConfig :: FilePath -> IO Config
readConfig configurationDir = do
  isDirectory <- doesDirectoryExist configurationDir
  unless isDirectory (putStrLn "Filepath must be a directory")
  lexnames' <- readTSV (configurationDir </> "lexnames.tsv") lexnamesReader
  return $ Config lexnames'
  where
    lexnamesReader [lexnameIdStr, lexicographerFile, _] =
      case TR.decimal lexnameIdStr of
        Left err -> Left err
        Right (lexnameId, "") -> Right (lexicographerFile, lexnameId)
        Right (_, trailing) -> Left $ "Trailing garbage after " ++ T.unpack trailing
    lexnamesReader _ = Left "Wrong number of fields"

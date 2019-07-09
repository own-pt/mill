module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    ) where

import Parse hiding (synsets)
import Validate

import Data.Either
--import Data.GenericTrie
--import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor(($>),void)
--import System.FilePath


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

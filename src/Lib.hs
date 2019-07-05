module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , validateLexicographerFile
    ) where

import Parse hiding (synsets)
import Process

import Data.Either
import Data.GenericTrie
--import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor(($>))
--import System.FilePath


parseLexicographerFile :: FilePath -> IO (Either () [SynsetToValidate])
parseLexicographerFile fileName = do
  content <- TIO.readFile fileName
  case parseLexicographer fileName content of
    Left err -> putStr err $> Left ()
    Right (_, _, lexFileSynsets) -> 
      return $ Right lexFileSynsets

parseLexicographerFiles :: [FilePath] -> IO (Validation [SourceError] (Index Synset))
parseLexicographerFiles fileNames = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile fileNames
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validateSynsetsInIndex index
    _ -> return (Failure [])

validateLexicographerFile :: FilePath -> [FilePath] -> IO [SourceError]
validateLexicographerFile fileToValidate otherFiles = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile (fileToValidate:otherFiles)
  case partitionEithers lexFilesSynsetsOrErrors of
    ([], lexFilesSynsets@(synsetsToValidate:_)) ->
      let synsets = concat lexFilesSynsets
          index   = makeIndex synsets
      in return $ validation id (const []) $ validateSynsets index synsetsToValidate
    _ -> return []

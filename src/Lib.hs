module Lib
    ( parseLexicographerFile
    ) where

import Parse
import Process
import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor(($>))
--import System.FilePath


parseLexicographerFile :: FilePath -> IO [Synset]
parseLexicographerFile fileName =
  do
    content <- TIO.readFile fileName
    case parseLexicographer fileName content of
      Left err -> putStr err $> []
      Right lexFileSynsetStatements ->
        return $ processLexicographerFile lexFileSynsetStatements

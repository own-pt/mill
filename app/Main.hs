module Main where

import Lib ( validateLexicographerFile
           , validateLexicographerFiles
           , lexicographerFilesInDirectoryToTriples)

import System.Directory (doesDirectoryExist)
import Options.Applicative

data Command = Validate FilePath
  | ExportCommand FilePath FilePath deriving Show

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseCommand :: Parser Command
parseCommand = subparser $
  -- validate
  command
   "validate"
   (info (helper <*> parseValidateCommand)
   (fullDesc <> progDesc "Validate lexicographer files"))
  <>
  -- export
  command
   "export"
   (info (helper <*> parseExportCommand)
   (fullDesc <> progDesc "Export lexicographer files"))

parseValidateCommand :: Parser Command
parseValidateCommand = Validate <$> validateParser
  where
    validateParser = argument str
      (metavar "PATH" <> help "Validates one lexicographer file in case PATH is a file, else validates all lexicographer files in directory. Assumes lexnames.tsv is in the same PATH")

parseExportCommand :: Parser Command
parseExportCommand = ExportCommand <$> lexDirectory <*> outputFile
  where
    lexDirectory = argument str
      (metavar "DIR" <> help "Directory where lexicographer files to export are in")
    outputFile = argument str
      (metavar "FILE" <> help "Output file path")

millProgDesc :: String
millProgDesc =
  "use mill to validate and export lexicographer files that make up a WordNet."

millHeader :: String
millHeader = "Manage lexicographer files that make up a WordNet."

main :: IO ()
main = do
  commandToRun <- showHelpOnErrorExecParser
    $ info (helper <*> parseCommand)
    (fullDesc <> progDesc millProgDesc <> header "wntext")
  case commandToRun of
    Validate filepath -> do
      isDirectory <- doesDirectoryExist filepath
      if isDirectory
        then validateLexicographerFiles filepath
        else validateLexicographerFile filepath
    (ExportCommand lexDirectory outputFile) ->
      lexicographerFilesInDirectoryToTriples lexDirectory outputFile

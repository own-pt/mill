module Main where

import Lib ( validateLexicographerFile
           , validateLexicographerFiles
           , lexicographerFilesJSON
           , readConfig )

import Control.Monad.Reader (ReaderT(..))
import Options.Applicative ( customExecParser, prefs, ParserInfo, Parser, showHelpOnError
                           , command, subparser, info, helper, fullDesc, progDesc
                           , argument, str, help, metavar, header )
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)

data Command = Validate FilePath
  | ExportJSONCommand FilePath FilePath
             deriving Show

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
parseExportCommand = jsonCommand
  where
    -- jsonFlag :: Parser ()
    -- jsonFlag = flag' () (long "json" <> help "Export to sensetion's JSON format")
    jsonCommand = --jsonFlag *>
      ExportJSONCommand <$> configDir <*> outputFile
    configDir = argument str
      (metavar "DIR" <> help "Directory where configuration files in")
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
      config <- readConfig $ takeDirectory filepath
      runReaderT (if isDirectory
                  then validateLexicographerFiles
                  else validateLexicographerFile filepath)
        config
    (ExportJSONCommand configDir outputFile) -> do
      config <- readConfig configDir
      runReaderT (lexicographerFilesJSON outputFile) config

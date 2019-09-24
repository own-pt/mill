module Main where

import Lib ( validateLexicographerFile
           , validateLexicographerFiles
           , lexicographerFilesJSON
           , readConfig
           , toWNDB )

import Control.Monad.Reader (ReaderT(..))
import Options.Applicative ( (<|>), argument, command, customExecParser, flag, fullDesc
                           , help, helper, info, long, metavar, header, Parser
                           , ParserInfo, prefs, progDesc, showHelpOnError, str, subparser )
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)

data ExportFormat = WNJSON | WNDB deriving (Show)

data Command = Validate FilePath
  | ExportCommand ExportFormat FilePath FilePath
             deriving (Show)

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
parseExportCommand = exportParser
  where
    jsonFlag = flag WNJSON WNJSON (long "json" <> help "Export to sensetion's JSON format [default]")
    wndbFlag = flag WNJSON WNDB   (long "wndb" <> help "Export to WNDB format")
    exportParser =
      ExportCommand <$> (jsonFlag <|> wndbFlag) <*> configDir <*> outputFile
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
      config <- readConfig $ if isDirectory then filepath else takeDirectory filepath
      runReaderT (if isDirectory
                  then validateLexicographerFiles
                  else validateLexicographerFile filepath)
        config
    (ExportCommand WNJSON configDir outputFile) -> do
      config <- readConfig configDir
      runReaderT (lexicographerFilesJSON outputFile) config
    (ExportCommand WNDB configDir outputFile) -> do
      config <- readConfig configDir
      runReaderT (toWNDB outputFile) config

module Main where

import Lib ( validateLexicographerFile
           , validateLexicographerFiles
           , lexicographerFilesJSON
           , readConfig
           , toWNDB )

import Control.Monad.Reader (ReaderT(..))
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative ( (<|>), argument, command, customExecParser, eitherReader, flag, fullDesc
                           , header, help, helper, info, long, metavar, option, Parser
                           , ParserInfo, prefs, progDesc, showHelpOnError
                           , ReadM, str, subparser, value
                           )
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)

type OneLanguage = Maybe Text

data ExportFormat = WNJSON | WNDB deriving (Show)

data Command = Validate OneLanguage FilePath
  | ExportCommand OneLanguage ExportFormat FilePath FilePath
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

parseOneLanguage :: ReadM OneLanguage
parseOneLanguage = eitherReader go
  where
    go "" = Left "Must specify a valid WordNet name"
    go lang = Right . Just . T.strip $ T.pack lang

oneLanguage :: String -> Parser OneLanguage
oneLanguage helpStr = option parseOneLanguage (long "one-lang" <> help helpStr <> value Nothing)
  
parseValidateCommand :: Parser Command
parseValidateCommand
  = Validate
  <$> oneLanguage "Ignore data from other wordnets in the validation"
  <*> validateParser
  where
    validateParser = argument str
      (metavar "PATH" <> help "Validates one lexicographer file in case PATH is a file, else validates all lexicographer files in directory. Assumes lexnames.tsv is in the same PATH")

parseExportCommand :: Parser Command
parseExportCommand = exportParser
  where
    jsonFlag = flag WNJSON WNJSON (long "json" <> help "Export to sensetion's JSON format [default]")
    wndbFlag = flag WNJSON WNDB   (long "wndb" <> help "Export to WNDB format")
    exportParser
      = ExportCommand
      <$> oneLanguage "Don't export data from other wordnets"
      <*> (jsonFlag <|> wndbFlag) <*> configDir <*> outputFile
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
    Validate oneLang filepath -> do
      isDirectory <- doesDirectoryExist filepath
      config <- readConfig oneLang $ if isDirectory then filepath else takeDirectory filepath
      runReaderT (if isDirectory
                  then validateLexicographerFiles
                  else validateLexicographerFile filepath)
        config
    (ExportCommand oneLang WNJSON configDir outputFile) -> do
      config <- readConfig oneLang configDir
      runReaderT (lexicographerFilesJSON outputFile) config
    (ExportCommand oneLang WNDB configDir outputFile) -> do
      config <- readConfig oneLang configDir
      runReaderT (toWNDB outputFile) config

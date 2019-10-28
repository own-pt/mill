module Main where

import Lib ( canonicalDir
           , validateLexicographerFile
           , validateLexicographerFiles
           , lexicographerFilesJSON
           , readConfig
           , toWNDB
           , Config(oneLang)
           )

import Data.Maybe (fromMaybe)
import Control.Monad.Reader (ReaderT(..))
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative ( (<|>), argument, command, customExecParser, eitherReader, flag, fullDesc
                           , header, help, helper, hsubparser, info, long, metavar, option, Parser
                           , ParserInfo, prefs, progDesc, showHelpOnError
                           , ReadM, short, str, value
                           )
import System.Directory (doesDirectoryExist)

type ConfigDir   = Maybe FilePath
type OneLanguage = Maybe Text

data ExportFormat = WNJSON | WNDB deriving (Show)

data MillCommand = MillCommand ConfigDir OneLanguage MillSubCommand

data MillSubCommand 
  = Validate FilePath
  | Export ExportFormat FilePath FilePath
             deriving (Show)

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseSubCommand :: Parser MillSubCommand -> Parser MillCommand
parseSubCommand subcommand = MillCommand <$> configDir <*> oneLanguage <*> subcommand
  where
    configDir = option (Just <$> str)
      (metavar "CONFIGDIR" <> short 'c' <> long "config-dir"
       <> help "Directory where configuration files are in"
       <> value Nothing)

parseCommand :: Parser MillCommand
parseCommand = hsubparser $
  -- validate
  command
   "validate"
   (info (helper <*> parseSubCommand parseValidateCommand)
   (fullDesc <> progDesc "Validate lexicographer files"))
  <>
  -- export
  command
   "export"
   (info (helper <*> parseSubCommand parseExportCommand)
   (fullDesc <> progDesc "Export lexicographer files"))

parseOneLanguage :: ReadM OneLanguage
parseOneLanguage = eitherReader go
  where
    go "" = Left "Must specify a valid WordNet name"
    go lang = Right . Just . T.strip $ T.pack lang

oneLanguage :: Parser OneLanguage
oneLanguage = option parseOneLanguage
  (metavar "LANG" <> short 'l' <> long "lang"
   <> help "Only consider WordNet LANG"
   <> value Nothing)
  
parseValidateCommand :: Parser MillSubCommand
parseValidateCommand
  = Validate <$> validateParser
  where
    validateParser = argument str
      (metavar "PATH" <> help validateHelp)

validateHelp :: String
validateHelp =
  "Validates one lexicographer file in case PATH is a file, else validates all lexicographer files in directory. If configuration directory was not provided, it is assumed to be in the same path."

parseExportCommand :: Parser MillSubCommand
parseExportCommand = exportParser
  where
    jsonFlag = flag WNJSON WNJSON (long "json" <> help "Export to JSON format [default]")
    wndbFlag = flag WNJSON WNDB   (long "wndb" <> help "Export to WNDB format")
    exportParser
      = Export <$> (jsonFlag <|> wndbFlag) <*> inPath <*> outputPath
    inPath = argument str
      (metavar "INPATH" <> help "Input path. If configuration directory was not provided, it is assumed to be in the same path.")
    outputPath = argument str
      (metavar "OUTPATH" <> help "Output path.")

millProgDesc :: String
millProgDesc =
  "use mill to validate and export lexicographer files that make up a WordNet."

millHeader :: String
millHeader = "Manage lexicographer files that make up a WordNet."

main :: IO ()
main = do
  commandToRun <- showHelpOnErrorExecParser
    $ info (helper <*> parseCommand)
    (fullDesc <> progDesc millProgDesc <> header "mill")
  case commandToRun of
    MillCommand configDir oneLang subcommand ->
      case subcommand of
        Validate inputPath -> validate configDir oneLang inputPath
        Export format inputPath outputPath
          -> export configDir oneLang format inputPath outputPath

getConfig :: ConfigDir -> OneLanguage -> FilePath -> IO Config
getConfig configDir' oneLang wnPath' = do
  wnPath    <- canonicalDir wnPath'
  configDir <- canonicalDir $ fromMaybe wnPath' configDir'
  readConfig oneLang wnPath configDir

validate :: ConfigDir -> OneLanguage -> FilePath -> IO ()
validate configDir oneLang inputPath = do
  config <- getConfig configDir oneLang inputPath
  isDirectory <- doesDirectoryExist inputPath
  runReaderT (if isDirectory
               then validateLexicographerFiles
               else validateLexicographerFile inputPath)
    config

export :: ConfigDir -> OneLanguage -> ExportFormat -> FilePath -> FilePath -> IO ()
export configDir oneLang format inputPath outputPath = do
  config <- getConfig configDir oneLang inputPath
  case format of
    WNJSON -> exportJSON outputPath config
    WNDB -> exportWNDB outputPath config

exportJSON :: FilePath -> Config -> IO ()
exportJSON outputFile = runReaderT $ lexicographerFilesJSON outputFile

exportWNDB :: FilePath -> Config -> IO ()
exportWNDB outputDir config =
  case oneLang config of
        Nothing -> error "You must specify a language for WNDB export"
        Just _ -> runReaderT (toWNDB outputDir) config

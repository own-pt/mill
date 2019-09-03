module Main where

import Lib (-- validateLexicographerFile
           validateLexicographerFile'
           , validateLexicographerFiles
           , lexicographerFilesJSON
           , lexicographerFilesInDirectoryToTriples
           , readConfig )

import Control.Monad.Reader (ReaderT(..))
import Options.Applicative ( customExecParser, prefs, ParserInfo, Parser, showHelpOnError
                           , command, subparser, info, helper, fullDesc, progDesc
                           , argument, str, help, metavar, header, long, flag'
                           , (<|>) )
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)

data Command = Validate FilePath
  | ExportRDFCommand String   -- baseIRI
                     FilePath -- lexicographer directory
                     FilePath -- output file
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
parseExportCommand = rdfCommand <|> jsonCommand
  where
    rdfFlag :: Parser ()
    rdfFlag = flag' () (long "rdf" <> help "Export to RDF")
    rdfCommand = rdfFlag
               *> (ExportRDFCommand <$> baseIRI <*> configDir <*> outputFile)
      where
        baseIRI = argument str
          (metavar "IRI" <> help "Base IRI for RDF nodes")
    jsonFlag :: Parser ()
    jsonFlag = flag' () (long "json" <> help "Export to sensetion's JSON format")
    jsonCommand = jsonFlag
                *> (ExportJSONCommand <$> configDir <*> outputFile)
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
                  else validateLexicographerFile' filepath)
        config
    (ExportRDFCommand baseIri lexDirectory outputFile) -> do
      config <- readConfig lexDirectory
      runReaderT (lexicographerFilesInDirectoryToTriples baseIri outputFile) config
    (ExportJSONCommand configDir outputFile) -> do
      config <- readConfig configDir
      runReaderT (lexicographerFilesJSON outputFile) config

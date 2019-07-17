module Main where

import Lib(validateLexicographerFile,validateLexicographerFiles)

import Options.Applicative

data ValidateCommand = ValidateFile FilePath
  | ValidateAll FilePath deriving Show

data ExportCommand = ExportCommand [FilePath] deriving Show

data Command = ValidateSubCommand ValidateCommand
  | ExportSubCommand ExportCommand deriving Show

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

parseCommand :: Parser Command
parseCommand = subparser $
  -- validate
  command
   "validate"
   (info (helper <*> (ValidateSubCommand <$> parseValidateCommand))
   (fullDesc <> progDesc "Validate lexicographer files"))
  <>
  -- export
  command
   "export"
   (info (helper <*> (ExportSubCommand <$> parseExportCommand))
   (fullDesc <> progDesc "Export lexicographer files"))

parseValidateCommand :: Parser ValidateCommand
parseValidateCommand =
      ValidateFile <$> validateFileParser
  <|> ValidateAll  <$> validateAllParser
  where
    validateFileParser = argument str
      (metavar "LEXFILE" <> help "Name of lexicographer file to validate")
    validateAllParser  = argument str
      (metavar "DIR" <> help "Directory where lexicographer files are in")

parseExportCommand :: Parser ExportCommand
parseExportCommand = ExportCommand <$> filePaths
  where
    filePaths = some $ argument str
      (metavar "LEXFILE" <> help "Name of lexicographer file to export")

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
    ValidateSubCommand (ValidateFile lexFile) -> validateLexicographerFile lexFile
    ValidateSubCommand (ValidateAll lexFilesDir) -> validateLexicographerFiles lexFilesDir
    _ -> return ()

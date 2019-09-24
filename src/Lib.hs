module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , build
    , validateLexicographerFile
    , validateLexicographerFiles
    , lexicographerFilesJSON
    , readConfig
    , toWNDB
    ) where

import Data ( Synset(..), Unvalidated, Validated
            , Validation(..), SourceValidation, singleton
            , SourceError(..), WNError(..), SourcePosition(..)
            , WNObj(..), readWNObj, WNPOS(..), readShortWNPOS
            , validate, validation )
import Export (synsetsToSynsetJSONs, calculateOffsets, showDBSynset)
import Parse (parseLexicographer)
import Validate ( makeIndex, Index, indexSynsets
                , validateSynsets, checkIndexNoDuplicates )
----------------------------------
import Control.Monad (unless,(>>),mapM,void)
import Control.Monad.Reader (ReaderT(..), ask, liftIO)
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (encodeFile,decodeFile)
import Data.ByteString.Builder (hPutBuilder)
import Data.Either (partitionEithers)
import Data.List (intercalate,find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Data.Text.Read as TR
import Development.Shake ( shake, ShakeOptions(..), shakeOptions
                         , need, want, (%>) )
import Development.Shake.FilePath ((<.>), (-<.>), takeFileName)
import System.Directory ( canonicalizePath, doesDirectoryExist
                        , doesDirectoryExist )
import System.FilePath ((</>), normalise, equalFilePath, takeDirectory, splitFileName)
import System.IO (BufferMode(..),withFile, IOMode(..),hSetBinaryMode,hSetBuffering)
import Debug.Trace

-- | This datastructure contains the information found in the
-- configuration files (currently only lexnames.tsv and relations.tsv
-- are considered.)
data Config = Config
  { -- | maps lexicographer filenames to their numerical ids
    lexnamesToId       :: Map Text Int
  -- | maps relation names in text files to their canonical names
  , textToCanonicNames :: Map Text Text
  -- | maps canonic relation names to their possible domains
  , canonicToDomain    :: Map Text (NonEmpty WNObj, NonEmpty WNPOS)
  -- | contains the filepaths to the files in the WordNet
  , lexFilePaths       :: NonEmpty FilePath
  } deriving (Show,Eq)

readTSV :: Ord a => Text -> ([Text] -> Either String [(a, b)]) -> IO (Map a b)
readTSV input readLine =
  case go input of
    ([], pairs) -> return . M.fromList $ concat pairs
    (errors, _) -> mapM_ putStrLn errors >> return M.empty
  where
    go = partitionEithers . map readLine
      . filter (not . isComment)
      . map (map T.strip . T.splitOn "\t") . drop 1 . T.lines
    isComment [field] = T.take 2 field == "--"
    isComment _ = False

readConfig :: FilePath -> IO Config
readConfig configurationDir' = do
  configurationDir   <- canonicalizePath configurationDir'
  isDirectory        <- doesDirectoryExist configurationDir
  unless isDirectory (error "Filepath must be a directory")
  go configurationDir
  where
    go configurationDir = do
      lexNamesInput      <- TIO.readFile $ configurationDir </> "lexnames.tsv"
      lexnamesToId       <- readTSV lexNamesInput lexnamesReader
      relationsInput     <- TIO.readFile $ configurationDir </> "relations.tsv"
      textToCanonicNames <- readTSV relationsInput textToCanonicNamesReader
      canonicToDomain    <- readTSV relationsInput canonicToDomainReader
      let lexFilePaths = lexFilePaths' lexnamesToId
      return $ Config {lexnamesToId, textToCanonicNames
                      , lexFilePaths, canonicToDomain
                      }
        where
          lexFilePaths' lexNamesMap =
            let lexnames = map normalize (M.keys lexNamesMap)
            in if null lexnames
            then error "No files specified in lexnames.tsv"
            else NE.fromList lexnames
          normalize lexFileId = configurationDir </> T.unpack lexFileId
          lexnamesReader [lexnameIdStr, lexicographerFile, _] =
            case TR.decimal lexnameIdStr of
              Left err -> Left err
              Right (lexnameId, "") -> Right [(lexicographerFile, lexnameId)]
              Right (_, trailing) -> Left $ "Trailing garbage after " ++ T.unpack trailing
          lexnamesReader _ = Left "Wrong number of fields in lexnames.tsv"
          textToCanonicNamesReader [_,_,"_",_,_,_,_]      = Right []
          textToCanonicNamesReader [canonicName,_,textName,_,_,_,_] = Right [(textName, canonicName)]
          textToCanonicNamesReader _ = Left "Wrong number of fields in relations.tsv"
          canonicToDomainReader [canonicName,_,_,_,pos,domain,_] =
            Right [(canonicName, ( readListField readWNObj domain
                                 , readListField readShortWNPOS pos))]
          canonicToDomainReader _ = Left "wrong number of Fiels in relations.tsv"
          readListField f = NE.fromList . map (f . T.strip) . T.splitOn ","
                                                                 

type App = ReaderT Config IO

parseLexicographerFile :: FilePath -> App (SourceValidation (NonEmpty (Synset Unvalidated)))
parseLexicographerFile filePath = do
  Config{textToCanonicNames, canonicToDomain} <- ask
  liftIO $ do
    content <- TIO.readFile $ normalise filePath
    let result = parseLexicographer textToCanonicNames canonicToDomain filePath content
    return result

parseLexicographerFiles ::  NonEmpty FilePath
  -> App (SourceValidation (NonEmpty (Synset Validated)))
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM parseLexicographerFile filePaths
  case sequenceA lexFilesSynsetsOrErrors of
    Success lexFilesSynsets ->
      let synsets = sconcat lexFilesSynsets
          validIndex   = checkIndexNoDuplicates $ makeIndex synsets
      in return $ validate (`validateSynsets` synsets) validIndex
    Failure sourceErrors -> return $ Failure sourceErrors

lexicographerFilesJSON :: FilePath -> App ()
lexicographerFilesJSON outputFile = do
  Config{lexFilePaths, lexnamesToId, textToCanonicNames} <- ask
  validationResults <- parseLexicographerFiles lexFilePaths
  case validationResults of
    Success synsets -> let jsonBuilder = synsetsToSynsetJSONs textToCanonicNames lexnamesToId synsets
                       in liftIO
                          $ withFile outputFile WriteMode (`write` jsonBuilder) 
    Failure errors -> liftIO $ prettyPrintList errors
  where
    write handle builder = do
      _ <- handle `hSetBinaryMode` True
      _ <- handle `hSetBuffering` BlockBuffering Nothing
      handle `hPutBuilder` builder
  

prettyPrintList :: Pretty a => NonEmpty a -> IO ()
prettyPrintList = mapM_ (putDoc . pretty)

getValidated :: App (SourceValidation (Index (Synset Unvalidated), NonEmpty (Synset Validated)))
getValidated = do
  _ <- build
  Config{lexFilePaths} <- ask
  validationIndices <- liftIO $ sequenceA <$> mapM readCachedIndex lexFilePaths
  let validationIndex = bimap id sconcat validationIndices
  case (validationIndex, validationIndices) of
    (Failure parseErrors, _)
      -> return $ Failure parseErrors
    (Success index, Success indices)
      -> case sconcat $ NE.map (go index) indices of
           Success validatedSynsets -> return $ Success (index, validatedSynsets)
           Failure errors -> return $ Failure errors
    (Success _, Failure impossible) -> return $ Failure impossible  -- never happens
  where
    go :: Index (Synset Unvalidated) -> Index (Synset Unvalidated) -> SourceValidation (NonEmpty (Synset Validated))
    go index targetIndex =
     case indexSynsets targetIndex of
       [] -> error "No synsets in target index" -- never happens
       (x:synsets) -> validateSynsets index (x:|synsets)

validateLexicographerFile :: FilePath -> App ()
-- | Validates lexicographer file without semantically validating all
-- other files (as long as they are syntactically valid)
validateLexicographerFile filePath = do
  normalFilePath   <- liftIO $ canonicalizePath filePath
  _ <- build
  Config{lexFilePaths} <- ask
  liftIO $
    case NE.partition (equalFilePath normalFilePath) lexFilePaths of
      ([_], otherFilePaths) -> liftIO $ go normalFilePath otherFilePaths
      ([], _) -> putStrLn $ "File " ++ normalFilePath ++ " is not specified in lexnames.tsv"
      _       -> putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go :: FilePath -> [FilePath] -> IO ()
    go fileToValidate lexFilePaths = do
      validationTargetFileIndex <- readCachedIndex fileToValidate
      -- FIXME: use unionWith instead of sconcat?
      validationIndex <- bimap id sconcat . sequenceA . (:|) validationTargetFileIndex
                      <$> mapM readCachedIndex lexFilePaths
      case (validationIndex, validationTargetFileIndex) of
        (Failure parseErrors, Success _)
          -> prettyPrintList $ toSingleError parseErrors
        (Failure parseErrors, Failure fileParseErrors)
          -> prettyPrintList (toSingleError parseErrors <> fileParseErrors)
        (Success _, Failure parseErrors)
          -> prettyPrintList parseErrors
        (Success index, Success targetIndex)
          -> case indexSynsets targetIndex of
               [] -> return ()
               (x:synsets) -> case validateSynsets index (x:|synsets) of
                                Success _ -> return ()
                                Failure validationErrors -> prettyPrintList validationErrors
    toSingleError
      = singleton
      . (\filesWithErrors -> SourceError (T.pack filePath) (SourcePosition (1,4))
          (ParseError $ "Errors in files: " ++ intercalate ", " (NE.toList filesWithErrors))) -- either parse errors or duplication errors
      . NE.nub
      . NE.map (\(SourceError fileWithErrors _ _) -> T.unpack fileWithErrors)

validateLexicographerFiles :: App ()
validateLexicographerFiles = do
  _ <- bimap prettyPrintList void <$> getValidated
  return ()

readCachedIndex :: FilePath
  -> IO (SourceValidation (Index (Synset Unvalidated)))
readCachedIndex lexFilePath = do
  let (directoryPath, filePath) = splitFileName lexFilePath
      indexPath = directoryPath </> ".cache" </> filePath <.> "index"
  decodeFile indexPath

---
-- cache
build :: App ()
build = do
  config@Config{lexFilePaths = lexFilePaths@(aLexFilePath:|_)} <- ask
  let cachePath = takeDirectory aLexFilePath </> ".cache"
  liftIO . shake shakeOptions{ shakeFiles=cachePath, shakeThreads=0 }
    $ do
    want . map (flip (<.>) "index" . (</>) cachePath . takeFileName)
      $ NE.toList lexFilePaths
    cachePath </> "*.index" %> \out -> do
        let lexFileName = takeFileName out -<.> ""
            maybeLexFilePath = find ((==) lexFileName . takeFileName) lexFilePaths
        case maybeLexFilePath of
          Nothing -> fail $ lexFileName ++ " not in lexnames.tsv"
          Just lexFilePath -> do
            need [lexFilePath]
            liftIO $ runReaderT (cacheFileIndex lexFilePath out) config
   where
     cacheFileIndex :: FilePath -> FilePath -> App ()
     cacheFileIndex lexFilePath outFilePath = do
       result <- parseLexicographerFile lexFilePath
       let validIndex = bimap id makeIndex result
       liftIO . encodeFile outFilePath
         $ validate checkIndexNoDuplicates validIndex

toWNDB :: FilePath -> App ()
toWNDB outputDir = do
  Config{lexnamesToId, textToCanonicNames} <- ask
  io <- validation prettyPrintList (go textToCanonicNames lexnamesToId) <$> getValidated
  _  <- liftIO io
  return ()
  where
    go :: Map Text Text -> Map Text Int -> (Index (Synset a), NonEmpty (Synset Validated)) -> IO ()
    go relationsMap lexicographerMap (index, synsets) =
      let (offsetMap, dbSynsets) = calculateOffsets 0 relationsMap lexicographerMap index synsets
          output = mconcat . NE.toList . NE.intersperse "\n" $ NE.map (showDBSynset offsetMap) dbSynsets
      in TIO.writeFile (trace outputDir outputDir) output

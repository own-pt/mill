module Lib
    ( parseLexicographerFile
    , parseLexicographerFiles
    , build
    , validateLexicographerFile
    , validateLexicographerFiles
    , lexicographerFilesJSON
    , lexicographerFilesToTriples
    , readConfig
    , lexicographerFilesInDirectoryToTriples
    ) where

import Data ( Synset(..), Unvalidated, Validated
            , Validation(..), SourceValidation, singleton
            , SourceError(..), WNError(..), SourcePosition(..)
            , WNObj(..), readWNObj, WNPOS(..), readShortWNPOS
            , validate )
import Export (synsetToTriples,synsetsToSynsetJSONs)
import Parse (parseLexicographer)
import Validate ( makeIndex, Index, indexSynsets
                , validateSynsets, checkIndexNoDuplicates )
----------------------------------
import Control.Monad (unless,(>>),mapM)
import Control.Monad.Reader (ReaderT(..), ask, liftIO)
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (encodeFile,decodeFile)
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Builder (hPutBuilder)
import qualified Data.DList as DL
import Data.Either (partitionEithers)
import Data.List (intercalate,find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.RDF.Encoder.NQuads (encodeRDFGraph)
import Data.RDF.ToRDF (runRDFGen)
import Data.RDF.Types (RDFGraph(..), IRI(..))
import Data.Semigroup (sconcat)
import Data.String (fromString)
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
                        , getXdgDirectory, XdgDirectory(..), doesDirectoryExist )
import System.FilePath ((</>), normalise,equalFilePath)
import System.IO (BufferMode(..),withFile, IOMode(..),hSetBinaryMode,hSetBuffering)

-- | This datastructure contains the information found in the
-- configuration files (currently only lexnames.tsv and relations.tsv
-- are considered.)
data Config = Config
  { -- | maps lexicographer filenames to their numerical ids
    lexnamesToId       :: Map Text Int
  -- | maps relation names in text files to their canonical names
  , textToCanonicNames :: Map Text Text
  -- | maps canonical relation names to their RDF names
  , canonicToRDFNames  :: Map Text Text
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
      canonicToRDFNames  <- readTSV relationsInput canonicToRDFNamesReader
      canonicToDomain    <- readTSV relationsInput canonicToDomainReader
      let lexFilePaths = lexFilePaths' lexnamesToId
      return $ Config {lexnamesToId, textToCanonicNames
                      , canonicToRDFNames, lexFilePaths, canonicToDomain
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
          canonicToRDFNamesReader  [canonicName,_,_,rdfName,_,_,_] = Right [(canonicName, rdfName)]
          canonicToRDFNamesReader _ = Left "Wrong number of fields in relations.tsv"
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
  Config{lexFilePaths, lexnamesToId} <- ask
  validationResults <- parseLexicographerFiles lexFilePaths
  case validationResults of
    Success synsets -> let jsonBuilder = synsetsToSynsetJSONs lexnamesToId synsets
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

getCachePath :: IO FilePath
getCachePath = getXdgDirectory XdgCache "mill"

validateLexicographerFile :: FilePath -> App ()
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
          (ParseError $ "ParseErrors in files: " ++ intercalate ", " (NE.toList filesWithErrors)))
      . NE.nub
      . NE.map (\(SourceError fileWithErrors _ _) -> T.unpack fileWithErrors)

validateLexicographerFiles :: App ()
validateLexicographerFiles = do
  Config{lexFilePaths} <- ask
  validationIndices <- liftIO $ sequenceA <$> mapM readCachedIndex lexFilePaths
  let validationIndex = bimap id sconcat validationIndices
  liftIO $ case (validationIndex, validationIndices) of
    (Failure parseErrors, _)
      -> prettyPrintList parseErrors
    (Success index, Success indices)
      -> mapM_ (go index) indices
    (Success _, Failure _) -> return () -- never happens
  where
    go index targetIndex =
     case indexSynsets targetIndex of
       [] -> return ()
       (x:synsets) -> case validateSynsets index (x:|synsets) of
         Success _ -> return ()
         Failure validationErrors -> prettyPrintList validationErrors
           

readCachedIndex :: FilePath
  -> IO (SourceValidation (Index (Synset Unvalidated)))
readCachedIndex lexFilePath = do
  cachePath <- getCachePath
  let indexPath  = cachePath </> takeFileName lexFilePath <.> "index"
  decodeFile indexPath

synsetsToTriples :: Map Text Text -> Map Text Text
  -> IRI -> NonEmpty (Synset Validated) -> FilePath -> IO ()
synsetsToTriples textToCanonicMap canonicToRDFMap baseIRI synsets outputFile =
  encodeFile outputFile
  . toLazyByteString
  . encodeRDFGraph . RDFGraph Nothing $ DL.toList synsetsTriples
  where
    synsetsTriples = foldMap (\synset -> runRDFGen
                               (synsetToTriples textToCanonicMap canonicToRDFMap synset)
                               baseIRI)
                     synsets

lexicographerFilesToTriples :: IRI -> FilePath -> App ()
lexicographerFilesToTriples baseIRI outputFile = do
  Config{textToCanonicNames, canonicToRDFNames, lexFilePaths} <- ask
  synsetsValid <- parseLexicographerFiles lexFilePaths
  liftIO $ case synsetsValid of
    Success synsets -> synsetsToTriples textToCanonicNames
                                          canonicToRDFNames baseIRI synsets outputFile
    Failure _ -> putStrLn "Errors in lexicographer files, please validate them before exporting."

lexicographerFilesInDirectoryToTriples :: String -> FilePath -> App ()
lexicographerFilesInDirectoryToTriples baseIriString =
  lexicographerFilesToTriples (fromString baseIriString)
  

---
-- cache
build :: App ()
build = do
  config@Config{lexFilePaths} <- ask
  cachePath <- liftIO getCachePath
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

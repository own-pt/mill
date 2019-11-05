module Lib
    ( Config(..)
    , canonicalDir
    , parseLexicographerFile
    , parseLexicographerFiles
    , build
   , validateLexicographerFile
   , validateLexicographerFiles
   , lexicographerFilesJSON
    , readConfig
--    , toWNDB
    ) where

import Data ( Synset(..), Unvalidated, Validated, OneWN
            , Validation(..), SourceValidation, singleton
            , SourceError(..), WNError(..), SourcePosition(..)
            , WNObj(..), readWNObj, WNPOS(..), readShortWNPOS
            -- , showLongWNPOS, synsetPOS
            , unsafeLookup
            -- , validate
            , validation )
import Export ( -- DBSynset(..), calculateOffsets, makeIndexIndex, newline
              -- , showDBSynset, showIndex,
                synsetsToSynsetJSONs )
import Parse (parseLexicographer)
import Validate ( makeIndex, ValIndex, SynsetMap, mapSynsets, removeInterWNRelations
                , validateSynsets )
----------------------------------
--import Debug.Trace (trace)
import Control.Monad (unless,mapM)
import Control.Monad.Reader (ReaderT(..), ask, liftIO)
--import Data.Bifunctor (Bifunctor(..))
import Data.Binary (encodeFile,decodeFileOrFail)
import Data.ByteString.Builder (hPutBuilder)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.List (intercalate, find--, intersperse
                 )
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
--import Data.Traversable (mapAccumL)
import Development.Shake ( shake, ShakeOptions(..), shakeOptions
                         , need, want, (%>) )
import Development.Shake.FilePath ((<.>), (-<.>), splitFileName, takeDirectory)
import System.Directory ( canonicalizePath, doesDirectoryExist--, createDirectoryIfMissing
                        , doesDirectoryExist, doesPathExist, withCurrentDirectory )
import System.FilePath ((</>), normalise, equalFilePath)
import System.IO (BufferMode(..),withFile, IOMode(..),hSetBinaryMode,hSetBuffering)


-- | This datastructure contains the information found in the
-- configuration files (currently only lexnames.tsv and relations.tsv
-- are considered.)
data Config = Config
  { -- | Only consider WordNet of name
    oneWN :: OneWN
  -- | contains the filepaths to the directory of each WordNet
  , wnPaths :: Map Text FilePath
  -- | maps lexicographer filenames to their numerical ids
  , lexnamesToId       :: Map Text Int
  -- | maps relation names in text files to their canonical names
  , textToCanonicNames :: Map Text Text
  -- | maps canonic relation names to their possible domains
  , canonicToDomain    :: Map Text (NonEmpty WNObj, NonEmpty WNPOS)
  -- | maps relation names in text files to their (original)
  -- lexicographer file names/symbols
  , textToLexRelations :: Map Text Text
  -- | where the index gets written, etc. In a multilanguage setting,
  -- only the files in this directory get written to output formats
  , mainWNDir :: FilePath
  } deriving (Show,Eq)


readTSV :: (Semigroup a, Ord a) => ([Text] -> Either String a) -> Text -> a
readTSV readLine input =
  case go input of
    ([], x:xs) -> sconcat (x:|xs)
    ([], []) -> error "No contents"
    (errors, _) -> error $ unlines errors 
  where
    go = partitionEithers
      . map readLine -- read contents
      . filter (not . isComment) -- remove comments
      . map (map T.strip . T.splitOn "\t") -- split into fields and strip whitespace
      . drop 1 -- remove header line
      . T.lines -- lines
    isComment [field] = T.take 2 field == "--"
    isComment _ = False

canonicalDir :: FilePath -> IO FilePath
canonicalDir filepath' = do
  filepath    <- canonicalizePath filepath'
  pathExists  <- doesPathExist filepath
  unless pathExists (error $ "Path " ++  filepath' ++ " does not exist")
  isDirectory <- doesDirectoryExist filepath
  return $ if isDirectory then filepath else takeDirectory filepath
  

readConfig :: Maybe Text -> FilePath -> FilePath -> IO Config
readConfig oneWN wnPath configurationDir = do
  lexNamesInput              <- readTSVwith (configurationDir </> "lexnames.tsv") lexnamesReader
  relationsInput             <- readTSVwith (configurationDir </> "relations.tsv") relationsReader
  (wnNames, wnRelativePaths) <- unzip <$> readTSVwith (configurationDir </> "wns.tsv") wnsReader
  wnAbsolutePaths            <- mapM toAbsolutePath wnRelativePaths
  let lexnamesToId       = toMap toLexnamesToId lexNamesInput
      textToCanonicNames = toMap toTextToCanonicName relationsInput
      canonicToDomain    = toMap toCanonicToDomain relationsInput
      textToLexRelations = toMap toTextToLexRelations relationsInput
      wnPaths            = toMap id $ zip wnNames wnAbsolutePaths
  return $ Config { oneWN, lexnamesToId, textToCanonicNames
                  , canonicToDomain, textToLexRelations
                  , mainWNDir = wnPath, wnPaths
                  }
  where
    readTSVwith filePath reader = readTSV reader <$> TIO.readFile filePath
    toMap f = M.fromList . map f
    toAbsolutePath relativePath
      = withCurrentDirectory wnPath (canonicalizePath $ T.unpack relativePath)
    toLexnamesToId (lexFileName, lexNameId) = (lexFileName, lexNameId)
    toTextToCanonicName (canonicName,_,textName,_) = (textName, canonicName)
    toCanonicToDomain (canonicName, _,_, domain) = (canonicName, domain)
    toTextToLexRelations (_,lexName, textName,_) = (textName, lexName)
    lexnamesReader [lexnameIdStr, lexicographerFile, _] =
      case TR.decimal lexnameIdStr of
        Left err -> Left err
        Right (lexnameId, "") -> Right [(lexicographerFile, lexnameId :: Int)]
        Right (_, trailing) -> Left $ "Trailing garbage after " ++ T.unpack trailing
    lexnamesReader _ = Left "Wrong number of fields in lexnames.tsv"
    relationsReader [_,_,"_",_,_,_,_] = Right []
    relationsReader [canonicName,lexName,textName,_,pos,domain,_]
      = Right [(canonicName, lexName, textName
               , (readListField readWNObj domain, readListField readShortWNPOS pos))
              ]
    relationsReader _ = Left "Wrong number of fields in relations.tsv"
    wnsReader [wnName, relativePath] = Right [(wnName, relativePath)]
    wnsReader _ = Left "Wrong number of fiels in wns.tsv"
    readListField f = NE.fromList . map (f . T.strip) . T.splitOn ","
                                                                 

type App = ReaderT Config IO


wnLexFiles :: App (NonEmpty (Text, FilePath))
wnLexFiles = do
  Config{wnPaths, oneWN, lexnamesToId} <- ask
  case oneWN of
    Just wnName -> return $ go lexnamesToId $ wnPath wnPaths wnName
    Nothing     -> return . sconcat . NE.fromList . map (go lexnamesToId . wnPath wnPaths) $ M.keys wnPaths
  where
    wnPath wnPaths wnName = ( wnName
                            , unsafeLookup ("No wordnet named " ++ T.unpack wnName) wnName wnPaths
                            )
    go lexnamesToId (wnName, wnDirPath) = NE.fromList . map (\lexname -> (wnName, wnDirPath </> T.unpack lexname)) $ M.keys lexnamesToId

wnFilePaths :: App (NonEmpty FilePath)
wnFilePaths = NE.map snd <$> wnLexFiles

parseLexicographerFile :: Text -> FilePath -> App (SourceValidation (NonEmpty (Synset Unvalidated)))
parseLexicographerFile wnName filePath = do
  Config{textToCanonicNames, canonicToDomain} <- ask
  liftIO $ do
    content <- TIO.readFile $ normalise filePath
    let result = parseLexicographer textToCanonicNames canonicToDomain wnName filePath content
    return result

parseLexicographerFiles :: NonEmpty (Text, FilePath)
  -> App (SourceValidation (NonEmpty (Synset Validated)))
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM (uncurry parseLexicographerFile) filePaths
  case sconcat lexFilesSynsetsOrErrors of
    Success synsets ->
      let (synsetMap, validIndex) = makeIndex synsets
      in return $ validateSynsets validIndex synsetMap synsets
    Failure sourceErrors -> return $ Failure sourceErrors

lexicographerFilesJSON :: FilePath -> App ()
lexicographerFilesJSON outputFile = do
  Config{lexnamesToId, textToCanonicNames} <- ask
  result <- getValidated
  case result of
    Failure errors -> liftIO $ prettyPrintList errors
    Success (_, _, synsets) -> let jsonBuilder = synsetsToSynsetJSONs textToCanonicNames lexnamesToId synsets
                               in liftIO
                               $ withFile outputFile WriteMode (`write` jsonBuilder) 
  where
    write handle builder = do
      _ <- handle `hSetBinaryMode` True
      _ <- handle `hSetBuffering` BlockBuffering Nothing
      handle `hPutBuilder` builder
  

prettyPrintList :: Pretty a => NonEmpty a -> IO ()
prettyPrintList = mapM_ (putDoc . pretty)

buildFiles :: App (NonEmpty FilePath)
buildFiles = build *> wnFilePaths

getValidated :: App (SourceValidation (SynsetMap Validated, ValIndex, NonEmpty (Synset Validated)))
-- | validate all lexicographer files and return the synset index and
-- a nonempty list of their validated synsets
getValidated = do
  lexFilePaths <- buildFiles
  Config{oneWN} <- ask
  validationCaches <- liftIO $ sequenceA <$> mapM (readCachedFileIndex oneWN) lexFilePaths
  let validationCache = fmap sconcat validationCaches
  case validationCache of
    Failure parseErrors
      -> return $ Failure parseErrors
    (Success (synsetMap, index))
      -> case go synsetMap index of
           Success validatedSynsets -> return $ Success (coerce synsetMap, index, validatedSynsets)
           Failure errors -> return $ Failure errors
  where
    go :: SynsetMap Unvalidated -> ValIndex
       -> SourceValidation (NonEmpty (Synset Validated))
    go synsetMap index =
     case mapSynsets synsetMap of
       [] -> error "No synsets in target index" -- never happens
       (x:synsets) -> validateSynsets index synsetMap (x:|synsets)

validateLexicographerFile :: FilePath -> App ()
-- | Validates lexicographer file without semantically validating all
-- other files (they must be syntactically valid, though)
validateLexicographerFile filePath = do
  Config{oneWN} <- ask
  normalFilePath <- liftIO $ canonicalizePath filePath
  lexFilePaths <- buildFiles
  liftIO $
    case NE.partition (equalFilePath normalFilePath) lexFilePaths of
      ([_], x:otherFilePaths) -> go oneWN normalFilePath (x:|otherFilePaths)
      ([_], []) -> go oneWN normalFilePath $ singleton normalFilePath
      ([], _) -> putStrLn $ "File " ++ normalFilePath ++ " is not specified in lexnames.tsv"
      _       -> putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go oneWN fileToValidate lexFilePaths = do
      validationTargetCache <- readCachedFileIndex oneWN fileToValidate
      validationOtherFileCaches <- sconcat
                                   <$> mapM (readCachedFileIndex oneWN) lexFilePaths
      case (validationOtherFileCaches, validationTargetCache) of
        (Failure parseErrors, Success _)
          -> prettyPrintList $ toSingleError parseErrors
        (Failure parseErrors, Failure fileParseErrors)
          -> prettyPrintList (toSingleError parseErrors <> fileParseErrors)
        (Success _, Failure parseErrors)
          -> prettyPrintList parseErrors
        (Success (othersSynsetMap, othersIndex), Success (targetSynsetMap, targetIndex))
          -> case mapSynsets targetSynsetMap of
               [] -> return ()
               (x:synsets)
                 -> case validateSynsets (targetIndex <> othersIndex)
                                         (targetSynsetMap <> othersSynsetMap)
                                         (x:|synsets) of
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
  ioAction <- validation prettyPrintList (return . const ()) <$> getValidated
  liftIO ioAction

readCachedFileIndex :: OneWN -> FilePath
  -> IO (SourceValidation (SynsetMap a, ValIndex))
-- | reads lexFile cached index at lexFilePath; if oneWN is not
-- Nothing, removes relations which point to targets who are part of
-- other WNs
readCachedFileIndex oneWN lexFilePath = do
  let (dirPath, filePath) = splitFileName lexFilePath
      indexPath = dirPath </> ".cache" </> filePath <.> "index"
  decodeResult <- decodeFileOrFail indexPath
  case decodeResult of
    Left (_, message) -> error $ unwords ["You probably have a corrupted cache; please delete it and try again\nFailed with message:", message]
    Right cached -> return $ fmap (\(synsetMap, index) -> (removeInterWNRelations oneWN synsetMap, index)) cached

---
-- cache
build :: App ()
build = do
  config@Config{mainWNDir} <- ask
  lexFiles <- wnLexFiles
  let shakeDir = mainWNDir </> ".cache"
  liftIO . shake shakeOptions{ shakeFiles=shakeDir, shakeThreads=0 }
    $ do
    -- targets
    want . map (cachePath . snd) $ NE.toList lexFiles
    -- rules
    "//.cache/*.index" %> \out -> do
        let (dirPath, fileName) = splitFileName $ out -<.> ""
            lexFilePath         = let dir = takeDirectory $ takeDirectory dirPath
                                  in dir </> fileName
            maybeLexFilePath = find ((==) lexFilePath . snd) lexFiles
        case maybeLexFilePath of
          Nothing -> fail $ lexFilePath ++ " not in lexnames.tsv"
          Just (wnName, _) -> do
            need [lexFilePath]
            liftIO $ runReaderT (cacheFileIndex wnName lexFilePath out) config
   where
     cachePath path =
       let (dirPath, fileName) = splitFileName path
       in dirPath </> ".cache" </> fileName <.> "index"
     cacheFileIndex :: Text -> FilePath -> FilePath -> App ()
     cacheFileIndex wnName lexFilePath outFilePath = do
       result <- parseLexicographerFile wnName lexFilePath
       let validIndex = fmap makeIndex result
       -- valid index has no duplicates, but may have invalid synsets
       liftIO $ encodeFile outFilePath validIndex

-- toWNDB :: FilePath -> App ()
-- toWNDB outputDir = do
--   Config{lexnamesToId, textToLexRelations} <- ask
--   _ <- liftIO $ createDirectoryIfMissing True outputDir
--   ioAction <- validation prettyPrintList (go textToLexRelations lexnamesToId) <$> getValidated
--   _  <- liftIO ioAction
--   return ()
--   where
--     go :: Map Text Text -> Map Text Int -> (ValIndex (Synset a), NonEmpty (Synset Validated)) -> IO ()
--     go relationsMap lexicographerMap (index, synsets) = do
--       mapM_ toData dbSynsetsByPOS
--       mapM_ toIndex [N,V,A,R] -- no S
--       where
--         synsetsByPOS = NE.groupWith1 (showLongWNPOS . synsetPOS) synsets
--         -- we need to calculate all offsets before writing the file
--         (offsetMap, dbSynsetsByPOS) = mapAccumL makeOffsetMap M.empty synsetsByPOS
--         indexIndex = makeIndexIndex index
--         write filename wnPos = TIO.writeFile (outputDir </> filename <.> T.unpack (showLongWNPOS wnPos))
--         makeOffsetMap currOffsetMap = calculateOffsets 0 currOffsetMap relationsMap lexicographerMap index
--         toData posDBsynsets@(x:|_) =
--           let output = mconcat . NE.toList . NE.intersperse newline $ NE.map (showDBSynset offsetMap) posDBsynsets
--           in write "data" (pos x) output
--         toIndex wnPOS =
--           let output = mconcat . intersperse newline $ showIndex wnPOS relationsMap offsetMap index indexIndex
--           in write "index" wnPOS output

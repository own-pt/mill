module Lib
    (
      App,
      Config(..),
      build,
      canonicalDir,
      hoist,
      lexicographerFilesJSON,
      parseLexicographerFile,
      parseLexicographerFiles,
      readConfig,
      toWNDB,
      validateLexicographerFile,
      validateLexicographerFiles,
      wnLexFiles,
    ) where

import Data ( Synset(..), Unvalidated, Validated
            , Validation(..), SourceValidation, singleton
            , SourceError(..), WNError(..), SourcePosition(..)
            , WNObj(..), readWNObj, WNPOS(..), readShortWNPOS
            , showLongWNPOS, synsetPOS, validate, validation )
import Export ( DBSynset(..), calculateOffsets, makeWndbIndex, newline
              , showDBSynset, showIndex, synsetsToSynsetJSONs, wndbSenseIndex )
import Parse (parseLexicographer)
import Validate ( makeIndex, Index, indexSynsets, oneLangIndex
                , validateSynsets, checkIndexNoDuplicates )
----------------------------------
--import Debug.Trace (trace)
import Control.Monad (foldM, mapM, unless)
import Control.Monad.RWS (RWST(..), ask, get, liftIO, mapRWST, put)
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (encodeFile, decodeFileOrFail)
import Data.ByteString.Builder (hPutBuilder)
import Data.Functor.Identity (Identity(..))
import Data.Either (partitionEithers)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import qualified Data.Text.Read as TR
import Data.Time.Calendar ()
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (mapAccumL)
import System.Directory ( canonicalizePath, doesDirectoryExist
                        , createDirectoryIfMissing, getModificationTime
                        , doesDirectoryExist, doesFileExist
                        , doesPathExist, withCurrentDirectory )
import System.FilePath ((</>), (<.>), normalise, equalFilePath, splitFileName, takeDirectory)
import System.IO (BufferMode(..),withFile, IOMode(..),hSetBinaryMode,hSetBuffering)


-- | This datastructure contains the information found in the
-- configuration files (currently only lexnames.tsv and relations.tsv
-- are considered.)
data Config = Config
  { -- | Only consider WordNet of name
    oneLang :: Maybe Text
  -- | contains the filepaths to the directory of each WordNet
  , wnPaths :: [(Text, FilePath)]
  -- | maps lexicographer filenames to their numerical ids
  , lexnamesToId :: Map Text Int
  -- | maps relation names in text files to their canonical names
  , textToCanonicNames :: Map Text Text
  -- | maps canonic relation names to their possible domains
  , canonicToDomain :: Map Text (NonEmpty WNObj, NonEmpty WNPOS)
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
readConfig oneLang wnPath configurationDir = do
  lexNamesInput              <- readTSVwith (configurationDir </> "lexnames.tsv") lexnamesReader
  relationsInput             <- readTSVwith (configurationDir </> "relations.tsv") relationsReader
  (wnNames, wnRelativePaths) <- unzip <$> readTSVwith (configurationDir </> "wns.tsv") wnsReader
  wnAbsolutePaths            <- mapM toAbsolutePath wnRelativePaths
  let lexnamesToId       = toMap toLexnamesToId lexNamesInput
      textToCanonicNames = toMap toTextToCanonicName relationsInput
      canonicToDomain    = toMap toCanonicToDomain relationsInput
      textToLexRelations = toMap toTextToLexRelations relationsInput
      wnPaths            = zip wnNames wnAbsolutePaths
  return $ Config { oneLang, lexnamesToId, textToCanonicNames
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
    wnsReader [wnName, relativePath, _] = Right [(wnName, relativePath)]
    wnsReader _ = Left "Wrong number of fiels in wns.tsv"
    readListField f = NE.fromList . map (f . T.strip) . T.splitOn ","

type BuildData = Maybe (NonEmpty (FilePath, UTCTime))
--data AppState = AppState {Index (Synset Validated)}
--- app state : files and timestamps, synset index, index relations
--- and reverse

--type WN = Index (Synset Validated)

type App' = RWST Config () BuildData
type App = App' IO
type AppI = App' Identity

unsafeAssoc :: Eq a => String -> a -> [(a,b)] -> b
unsafeAssoc errorMsg key = fromMaybe (error errorMsg) . lookup key

unsafeRassoc :: Eq b => String -> b -> [(a,b)] -> a
unsafeRassoc errorMsg key = fromMaybe (error errorMsg) . rlookup key
  where
    rlookup _key []          =  Nothing
    rlookup  k ((x,y):xys)
        | k == y          =  Just x
        | otherwise       =  rlookup k xys


wnLexFiles :: AppI (NonEmpty (Text, FilePath))
wnLexFiles = do
  Config{wnPaths, oneLang, lexnamesToId} <- ask
  case oneLang of
    Just wnName -> return $ go lexnamesToId $ wnPath wnPaths wnName
    Nothing     -> return . sconcat . NE.fromList . map (go lexnamesToId . wnPath wnPaths) $ map fst wnPaths
  where
    wnPath wnPaths wnName = ( wnName
                            , unsafeAssoc ("No wordnet named " ++ T.unpack wnName) wnName wnPaths
                            )
    go lexnamesToId (wnName, wnDirPath) = NE.fromList . map (\lexname -> (wnName, wnDirPath </> T.unpack lexname)) $ M.keys lexnamesToId

wnFilePaths :: AppI (NonEmpty FilePath)
wnFilePaths = NE.map snd <$> wnLexFiles

parseLexicographerFile :: Text -> FilePath -> App (SourceValidation (NonEmpty (Synset Unvalidated)))
parseLexicographerFile wnName filePath = do
  Config{textToCanonicNames, canonicToDomain} <- ask
  content <- liftIO . TIO.readFile $ normalise filePath
  return $ parseLexicographer textToCanonicNames canonicToDomain wnName filePath content

parseLexicographerFiles :: NonEmpty (Text, FilePath)
  -> App (SourceValidation (NonEmpty (Synset Validated)))
parseLexicographerFiles filePaths = do
  lexFilesSynsetsOrErrors <- mapM (uncurry parseLexicographerFile) filePaths
  case sconcat lexFilesSynsetsOrErrors of
    Success synsets ->
      let validIndex = checkIndexNoDuplicates $ makeIndex synsets
      in return $ validate (`validateSynsets` synsets) validIndex
    Failure sourceErrors -> return $ Failure sourceErrors

lexicographerFilesJSON :: FilePath -> App ()
lexicographerFilesJSON outputFile = do
  Config{lexnamesToId, textToCanonicNames} <- ask
  result <- getValidated
  case result of
    Failure errors -> liftIO $ prettyPrintList errors
    Success (index, synsets) ->
      let jsonBuilder = synsetsToSynsetJSONs index textToCanonicNames lexnamesToId synsets
      in liftIO $ withFile outputFile WriteMode (`write` jsonBuilder)
  where
    write handle builder = do
      _ <- handle `hSetBinaryMode` True
      _ <- handle `hSetBuffering` BlockBuffering Nothing
      handle `hPutBuilder` builder


prettyPrintList :: Pretty a => NonEmpty a -> IO ()
prettyPrintList = mapM_ (putDoc . pretty)

hoist :: AppI a -> App a
hoist = mapRWST (return.runIdentity)

buildFiles :: App (NonEmpty FilePath)
buildFiles = build *> hoist wnFilePaths

getValidated :: App (SourceValidation (Index (Synset Unvalidated), NonEmpty (Synset Validated)))
-- | validate all lexicographer files and return the synset index and
-- a nonempty list of their validated synsets
getValidated = do
  lexFilePaths <- buildFiles
  Config{oneLang} <- ask
  -- we first read the indices from the cache and sequence them; we
  -- can't merge them now because we need to validate the synsets from
  -- each file as a unit, or else we get spurious sort errors
  validationIndices <- liftIO $ sequenceA <$> mapM (readCachedFileIndex oneLang) lexFilePaths
  let validationIndex = bimap id sconcat validationIndices
  case (validationIndex, validationIndices) of
    (Failure parseErrors, _)
      -> return $ Failure parseErrors
    (Success index, Success indices)
      -> case sconcat $ NE.map (validateFileSynsets index) indices of
           Success validatedSynsets -> return $ Success (index, validatedSynsets)
           Failure errors -> return $ Failure errors
    (Success _, Failure impossible) -> return $ Failure impossible  -- never happens
  where
    validateFileSynsets :: Index (Synset Unvalidated) -> Index (Synset Unvalidated)
       -> SourceValidation (NonEmpty (Synset Validated))
    validateFileSynsets index targetIndex =
     case indexSynsets targetIndex of
       [] -> error "No synsets in target index" -- never happens
       (x:synsets) -> validateSynsets index (x:|synsets)

validateLexicographerFile :: FilePath -> App ()
-- | Validates lexicographer file without semantically validating all
-- other files (they must be syntactically valid, though)
validateLexicographerFile filePath = do
  Config{oneLang} <- ask
  normalFilePath <- liftIO $ canonicalizePath filePath
  lexFilePaths <- buildFiles
  liftIO $ case NE.partition (equalFilePath normalFilePath) lexFilePaths of
    ([_], otherFilePaths) -> go oneLang normalFilePath otherFilePaths
    ([], _) -> putStrLn $ "File " ++ normalFilePath ++ " is not specified in lexnames.tsv"
    _       -> putStrLn $ "File " ++ normalFilePath ++ " is doubly specified in lexnames.tsv"
  where
    go oneLang fileToValidate lexFilePaths = do
      validationTargetFileIndex <- readCachedFileIndex oneLang fileToValidate
      -- FIXME: use unionWith instead of sconcat?
      validationOtherFileIndices <- sequenceA
                                 <$> mapM (readCachedFileIndex oneLang) lexFilePaths
      case (validationOtherFileIndices, validationTargetFileIndex) of
        (Failure parseErrors, Success _)
          -> prettyPrintList $ toSingleError parseErrors
        (Failure parseErrors, Failure fileParseErrors)
          -> prettyPrintList (toSingleError parseErrors <> fileParseErrors)
        (Success _, Failure parseErrors)
          -> prettyPrintList parseErrors
        (Success otherFileIndices, Success targetIndex)
          -> case indexSynsets targetIndex of
               [] -> return ()
               (x:synsets)
                 -> case validateSynsets (sconcat $ targetIndex:|otherFileIndices)
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

readCachedFileIndex :: Maybe Text -> FilePath
  -> IO (SourceValidation (Index (Synset Unvalidated)))
-- | reads lexFile cached index at lexFilePath; if oneLang is not
-- Nothing, removes relations which point to targets who are part of
-- other WNs
readCachedFileIndex oneLang lexFilePath = do
  let (dirPath, filePath) = splitFileName lexFilePath
      indexPath = dirPath </> ".cache" </> filePath <.> "index"
  decodeResult <- decodeFileOrFail indexPath
  case decodeResult of
    Left (_, message) -> error $ unwords ["You probably have a corrupted cache; please delete it and try again\nFailed with message:", message]
    Right index' -> return $ bimap id (oneLangIndex oneLang) index'

---
-- cache
build :: App ()
build = do
  maybeBuildData <- get
  buildData <- case maybeBuildData :: Maybe (NonEmpty (FilePath, UTCTime)) of
    Nothing -> hoist wnFilePaths >>= liftIO . mapM getBuildData
    Just buildData -> return buildData
  (toUpdate, buildData') <- liftIO . foldM update ([], []) $ NE.toList buildData
  put . Just $ NE.fromList buildData'
  mapM_ cacheFileIndex toUpdate
  where
    getBuildData fp = do
      let cacheFilePath = cachePath fp
      fileExists <- doesFileExist cacheFilePath
      (fp,) <$>
        if fileExists
        then getModificationTime cacheFilePath
        else return $ UTCTime (toEnum 0) (toEnum 0)
    update (toUpdate, buildData) (fp, modTime) = do
      modTime' <- getModificationTime fp
      let toUpdate' = if modTime < modTime' then fp:toUpdate else toUpdate
      return (toUpdate', (fp, modTime'):buildData)
    cacheFileIndex :: FilePath -> App ()
    cacheFileIndex lexFilePath = do
      liftIO . putStrLn $ "Rebuilding " ++ lexFilePath
      Config{wnPaths} <- ask
      let wnName = unsafeRassoc (lexFilePath ++ " is not in a WN dir") (takeDirectory lexFilePath) wnPaths
      result <- parseLexicographerFile wnName lexFilePath
      -- valid index has no duplicates, but may have invalid synsets
      let validIndex = bimap id makeIndex result
      let outFilePath = cachePath lexFilePath
      liftIO . createDirectoryIfMissing True $ takeDirectory outFilePath
      liftIO . encodeFile outFilePath
        $ validate checkIndexNoDuplicates validIndex
    cachePath path =
      let (dirPath, fileName) = splitFileName path
      in dirPath </> ".cache" </> fileName <.> "index"


toWNDB :: FilePath -> App ()
toWNDB outputDir = do
  Config{lexnamesToId, textToLexRelations} <- ask
  _ <- liftIO $ createDirectoryIfMissing True outputDir
  ioAction <- validation prettyPrintList (go textToLexRelations lexnamesToId) <$> getValidated
  _  <- liftIO ioAction
  return ()
  where
    go :: Map Text Text -> Map Text Int -> (Index (Synset a), NonEmpty (Synset Validated)) -> IO ()
    go relationsMap lexicographerMap (index, synsets) = do
      mapM_ toData dbSynsetsByPOS
      mapM_ toIndex [N,V,A,R] -- no S
      senseIndex
      where
        synsetsByPOS = NE.groupWith1 (showLongWNPOS . synsetPOS) synsets
        -- we need to calculate all offsets before writing the file
        (offsetMap, dbSynsetsByPOS) = mapAccumL makeOffsetMap M.empty synsetsByPOS
        indexIndex = makeWndbIndex index
        write filename wnPos = TIO.writeFile (outputDir </> filename <.> T.unpack (showLongWNPOS wnPos))
        makeOffsetMap currOffsetMap = calculateOffsets 0 currOffsetMap relationsMap lexicographerMap index
        toData posDBsynsets@(x:|_) =
          let output = mconcat . NE.toList . NE.intersperse newline $ NE.map (showDBSynset offsetMap) posDBsynsets
          in write "data" (pos x) output
        toIndex wnPOS =
          let output = mconcat . intersperse newline $ showIndex wnPOS relationsMap offsetMap index indexIndex
          in write "index" wnPOS output
        senseIndex =
          let output = mconcat . intersperse newline $ wndbSenseIndex lexicographerMap index offsetMap
          in TIO.writeFile (outputDir </> "index.sense") output

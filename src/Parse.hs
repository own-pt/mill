module Parse (parseLexicographer) where

import Data

import Control.Applicative hiding (some,many)
import qualified Control.Applicative.Combinators.NonEmpty as NC
import Control.Monad (void)
import Control.Monad.Reader (Reader,reader,runReader)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, string, eol)
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Megaparsec.Debug (dbg)
import qualified Data.Set as S

fst3 :: (a,b,c) -> a
fst3 (first,_,_) = first

type RawSynset = Either (ParseError Text Void) (Synset Unvalidated)

-- State stores in which lexicographer file we're in, this is useful
-- to fill in implicit references
type Parser = ParsecT Void Text (Reader (LexicographerFileId, Map Text Text, Map Text (NonEmpty WNObj, NonEmpty WNPOS)))

parseLexicographer :: Map Text Text
  -> Map Text (NonEmpty WNObj, NonEmpty WNPOS)
  -> String -> Text
  -> SourceValidation (NonEmpty (Synset Unvalidated))
parseLexicographer textToCanonicNames canonicToDomain fileName inputText =
  case runReader (runParserT lexicographerFile fileName inputText)
                 (lexFileId, textToCanonicNames, canonicToDomain) of
    Right rawSynsets
      -> case partitionEithers (NE.toList rawSynsets) of
           ([], synsetsToValidate) -> Success $ NE.fromList synsetsToValidate
           (parseErrors, _) -> Failure . NE.map parseToSourceError
                                           $ NE.fromList parseErrors
    Left ParseErrorBundle{bundleErrors} ->
      Failure . NE.map parseToSourceError $ bundleErrors
  where
    parseToSourceError parseError
      = let errorPos = errorOffset parseError
        in SourceError (T.pack fileName)
                       (SourcePosition (errorPos, errorPos +1))
                       (ParseError $ parseErrorTextPretty parseError)
    lexFileId :: LexicographerFileId
    lexFileId  =
      fromMaybe (error $ "Couldn't parse first line of "
                 ++ fileName ++ " as lexicographer file id") .
                 lexicographerFileIdFromText . T.strip $ headerText
    headerText = T.takeWhile (/= '\n') inputText

lexicographerIdP :: Parser LexicographerFileId
lexicographerIdP = do
  pos     <- posP
  _       <- char '.'
  lexname <- lexnameP
  return $ LexicographerFileId (pos, lexname)
  where
    posP = N <$ string "noun"
       <|> V <$ string "verb"
       <|> S <$ string "adjs"
       <|> A <$ string "adj"
       <|> R <$ string "adv"
    lexnameP = lexeme (takeWhile1P Nothing (`notElem` [' ','\t',':','\n'])
                 <?> "Lexicographer file name (must not contain whitespace or a colon)")

lexicographerFile :: Parser (NonEmpty RawSynset)
lexicographerFile = do
  _          <- spaceConsumer
  _          <- lexicographerIdP
  _          <- many linebreaks
  rawSynsets <- synsets
  _          <- eof
  return rawSynsets

synsets :: Parser (NonEmpty RawSynset)
synsets = synsetOrError `NC.sepEndBy1` many linebreak
  where
    synsetOrError = withRecovery recover (Right <$> synset)
    recover :: ParseError Text Void -> Parser RawSynset
    recover err = Left err <$ manyTill anySingle (try $ count 2 linebreak)

synset :: Parser (Synset Unvalidated)
synset = do
  startOffset      <- (1 +) <$> getOffset
  lexicographerId  <- reader fst3
  synsetWordSenses <- wordSenseStatement `NC.endBy1` linebreak
  synsetDefinition <- definitionStatement <* linebreak
  synsetExamples   <- exampleStatement `endBy` linebreak
  synsetFrames     <- option [] (framesStatement <* linebreak)
  synsetRelations  <- synsetRelationStatement `endBy` linebreak
  endOffset        <- getOffset
  return $ Synset (SourcePosition (startOffset, endOffset)) lexicographerId synsetWordSenses synsetDefinition synsetExamples synsetFrames synsetRelations

spaceConsumer :: Parser ()
-- comments will only be allowed as statements, because we want to
-- serialize to this format too
spaceConsumer = L.space spaces empty empty
  where
    spaces = void $ takeWhile1P Nothing (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

integer :: Parser Int
integer = lexeme L.decimal

statement :: Text -> Parser a -> Parser a
statement name parser = L.nonIndented spaceConsumer go
  where
    go = symbol name *> symbol ":" *> parser

definitionStatement :: Parser Text
definitionStatement = statement "d" textBlock

exampleStatement :: Parser Text
exampleStatement = statement "e" textBlock

synsetRelationStatement :: Parser SynsetRelation
synsetRelationStatement = L.nonIndented spaceConsumer go
  where
    go = SynsetRelation
      <$> relationNameP SynsetObj synsetRelationName
      <*> (SynsetIdentifier <$> identifier)
    synsetRelationName = T.stripEnd
      -- [ ] handle this better
      <$> (takeWhile1P Nothing (`notElem` [':', ' ', '\n']) <?> "Synset relation name")
      <* symbol ":"

relationNameP :: WNObj -> Parser Text -> Parser Text
relationNameP obj name = do
  LexicographerFileId (wnPOS, _) <- reader fst3
  relationsMap     <- reader $ \(_,second,_) -> second
  relationsDomains <- reader $ \(_,_,third) -> third
  relationName     <- name
  let relationString = T.unpack relationName
  case M.lookup relationName relationsMap of
    Just canonicName -> if relationName `elem` ["d", "e", "fs", "w"]
              then fail "Synset components must come in the following order: words, definition, examples, frames, and synset relations"
              else case M.lookup canonicName relationsDomains of
                     Just (domain,poses) -> case (obj `elem` domain, wnPOS `elem` poses) of
                       (True,True)  -> return relationName
                       _            -> fail $ relationString
                                            ++ " is not a valid relation for a " ++ show obj
                                            ++ " with PoS " ++ show wnPOS
                     Nothing -> error $ "Can't find " ++ T.unpack relationName ++ "'s domain"
    Nothing -> failure (Just $ toErrorItem relationName)
                       (S.fromList . map toErrorItem $ M.keys relationsMap)
  where
    toErrorItem = Label . NE.fromList . show

wordSenseStatement :: Parser WNWord
wordSenseStatement = statement "w" go
  where
    go = WNWord <$> wordSenseIdentifier <*> wordSenseFrames <* wordSenseMarker
                <*> wordSensePointers
    wordSenseFrames = option [] $ symbol "fs" *> frameNumbers
    wordSenseMarker = optional $ symbol "marker" *> word

wordSenseIdentifier :: Parser WordSenseIdentifier
wordSenseIdentifier = WordSenseIdentifier <$> identifier

identifier :: Parser (LexicographerFileId, WordSenseForm, LexicalId)
identifier =
  (,,)
  <$>  (try lexicographerIdentifier <|> reader fst3)
  <*>  fmap WordSenseForm word <*> lexicalIdentifier
  where
    lexicographerIdentifier = do
      lexId <- lexicographerIdP
      _ <- string ":"
      return lexId

wordSensePointers :: Parser [WordPointer]
wordSensePointers = many go
  where
    go =  WordPointer
      <$> relationNameP WordObj (word <?> "Word pointer")
      <*> wordSenseIdentifier

word :: Parser Text
word = lexeme $ takeWhile1P Nothing (not . isSpace)

lexicalIdentifier :: Parser LexicalId
lexicalIdentifier = LexicalId <$> option 0 (integer <?> "Lexical Identifier")

framesStatement :: Parser [Int]
framesStatement = statement "fs" frameNumbers

frameNumbers :: Parser [Int]
frameNumbers = some (integer <?> "Frame number")

lineText :: Parser Text
lineText = T.stripEnd <$> takeWhileP Nothing (/= '\n')

textBlock :: Parser Text
textBlock = T.unwords <$> lineText `sepBy1` try (eol *> some (char ' '))

linebreak :: Parser ()
linebreak = void $ lexeme eol

linebreaks :: Parser ()
linebreaks = void $ some linebreak


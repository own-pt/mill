module Parse where

import Data

import Control.Applicative hiding (some,many)
import qualified Control.Applicative.Combinators.NonEmpty as NC
import Control.Monad (void)
import Control.Monad.Reader (Reader,runReader,ask)
import Data.Char
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Megaparsec.Debug (dbg)


type RawSynset = Either (ParseError Text Void) (Synset Unvalidated)

-- State stores in which lexicographer file we're in, this is useful
-- to fill in implicit references
type Parser = ParsecT Void Text (Reader LexicographerFileId)

parseLexicographer :: String -> Text -> Text
  -> Either String [Synset Unvalidated]
parseLexicographer fileName lexicographerFileId inputText =
  case runReader (runParserT lexicographerFile fileName inputText)
                 (LexicographerFileId lexicographerFileId)
  of
    Right rawSynsets
      -> case partitionEithers rawSynsets of
           ([], synsetsToValidate) -> Right synsetsToValidate
           (parseErrors, _) -> Left . errorBundlePretty
                                 $ ParseErrorBundle (NE.fromList parseErrors) initialPosState
    Left errors -> Left $ errorBundlePretty errors
  where
    initialPosState = PosState
      { pstateInput = inputText
      , pstateOffset = 0
      , pstateSourcePos = initialPos fileName
      , pstateTabWidth = defaultTabWidth
      , pstateLinePrefix = ""
      }


lexicographerFile :: Parser [RawSynset]
lexicographerFile = do
  _ <- spaceConsumer
  _ <- many linebreaks
  rawSynsets <- synsets
  _ <- eof
  return rawSynsets

synsets :: Parser [RawSynset]
synsets = synsetOrError `sepEndBy1` many linebreak
  where
    synsetOrError = withRecovery recover (Right <$> synset)
    recover :: ParseError Text Void -> Parser RawSynset
    recover err = Left err <$ manyTill anySingle (try $ count 2 linebreak)

synset :: Parser (Synset Unvalidated)
synset = do
  startOffset <- getOffset
  lexicographerId <- ask
  synsetWordSenses <- wordSenseStatement `NC.endBy1` linebreak
  synsetDefinition <-  definitionStatement <* linebreak
  synsetExamples <- exampleStatement `endBy` linebreak
  synsetFrames <- option [] (framesStatement <* linebreak)
  synsetRelations <- synsetRelationStatement `endBy` linebreak
  endOffset <- getOffset
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
    go = SynsetRelation <$> relationName <*> (SynsetIdentifier <$> identifier)
    relationName = T.stripEnd
      -- [ ] handle this better
      <$> takeWhile1P (Just "Synset relation name") (`notElem` [':', ' ', '\n'])
      <* symbol ":"

wordSenseStatement :: Parser WNWord
wordSenseStatement = statement "w" go
  where
    go = WNWord <$> wordSenseIdentifier <*> wordSenseFrames <*> wordSensePointers

wordSenseIdentifier :: Parser WordSenseIdentifier
wordSenseIdentifier = WordSenseIdentifier <$> identifier

identifier :: Parser (LexicographerFileId, WordSenseForm, LexicalId)
identifier =
  (,,) <$>
  (try lexicographerIdentifier <|> ask) <*> fmap WordSenseForm word <*> lexicalIdentifier
  where
    lexicographerFilePos :: Parser Text
    lexicographerFilePos = choice $
      map string ["noun", "verb", "adjs", "adj", "adv"]
    lexicographerFileName = takeWhile1P Nothing (/= ':')
    lexicographerIdentifier :: Parser LexicographerFileId
    lexicographerIdentifier = do
      pos <- lexicographerFilePos
      _ <- string "."
      fileName <- lexicographerFileName
      _ <- string ":"
      return $ LexicographerFileId $ T.concat [pos,".",fileName]

wordSensePointers :: Parser [WordPointer]
wordSensePointers = many go
  where
    go = WordPointer <$> (word <?> "Word pointer") <*> wordSenseIdentifier

wordSenseFrames :: Parser [Int]
wordSenseFrames = option [] $ symbol "frames" *> frameNumbers

word :: Parser Text
word = lexeme $ takeWhile1P Nothing (not . isSpace)

lexicalIdentifier :: Parser LexicalId
lexicalIdentifier = LexicalId <$> option 0 (integer <?> "Lexical Identifier")

framesStatement :: Parser [Int]
framesStatement = statement "fs" frameNumbers

frameNumbers :: Parser [Int]
frameNumbers = some (integer <?> "Frame number")

whiteSpaceConsumer :: Parser ()
whiteSpaceConsumer = L.space (void spaceChar) empty empty

lineText :: Parser Text
lineText = T.stripEnd <$> takeWhileP Nothing (/= '\n')

textBlock :: Parser Text
textBlock = T.unwords <$> lineText `sepBy1` try (eol *> some (char ' '))

linebreak :: Parser ()
linebreak = void $ lexeme eol

linebreaks :: Parser ()
linebreaks = void $ some linebreak


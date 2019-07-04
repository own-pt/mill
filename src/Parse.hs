{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative hiding (some,many)
import Control.Monad.State.Strict
import Data.Char
import Data.List.NonEmpty(NonEmpty)
import Data.Either
import Data.Text (Text)
import Data.Void (Void)
import qualified Control.Applicative.Combinators.NonEmpty as NC
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Megaparsec.Debug (dbg)

---
type WordSenseIdentifier = ( Text -- ^ Lexicographer file identifier
                           , Text -- ^ Word form
                           , Int  -- ^ Lexical identifier
                           )

type SynsetIdentifier = WordSenseIdentifier
type PointerName = Text
type RelationName = Text
data WordPointer = WordPointer PointerName WordSenseIdentifier
  deriving (Show,Eq)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Show,Eq)
type FrameIdentifier = Int
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Show,Eq)

type RawSynset = Either (ParseError Text Void) SynsetToValidate

data SynsetToValidate = SynsetToValidate
  { sourcePosition       :: Int
  , lexicographerFileId  :: Text
  , wordSenses           :: NonEmpty WNWord
  , definition           :: Text
  , examples             :: [Text]
  , frames               :: [Int]
  , relations            :: NonEmpty SynsetRelation
  } deriving (Show,Eq)
---


-- State stores in which lexicographer file we're in, this is useful
-- to fill in implicit references
type Parser = ParsecT Void Text (State Text)

parseLexicographer :: String -> Text
  -> Either String (Text, Int, [SynsetToValidate])
parseLexicographer fileName inputText =
  case evalState (runParserT lexicographerFile fileName inputText) "all.all" of
    Right (lexicographerFileName, lexicographerIdentifier, rawSynsets)
      -> case partitionEithers rawSynsets of
           ([], synsetsToValidate) ->
             Right (lexicographerFileName, lexicographerIdentifier, synsetsToValidate)
           (parseErrors, _) -> Left $ concatMap parseErrorPretty parseErrors
    Left errors -> Left $ errorBundlePretty errors


lexicographerFile :: Parser (Text, Int, [RawSynset])
lexicographerFile = do
  _ <- spaceConsumer
  lexicographerFileName <- word <?> "Lexicographer file name"
  lexicographerFileNumber <- integer <?> "Lexicographer file identifier"
  put lexicographerFileName
  _ <- linebreaks
  rawSynsets <- synsets
  _ <- eof
  return (lexicographerFileName, lexicographerFileNumber, rawSynsets)

synsets :: Parser [RawSynset]
synsets = synsetOrError `sepEndBy1` many linebreak
  where
    synsetOrError = withRecovery recover (Right <$> synset)
    recover :: ParseError Text Void -> Parser RawSynset
    recover err = Left err <$ manyTill anySingle (try $ count 2 linebreak)

synset :: Parser SynsetToValidate
synset = SynsetToValidate
  <$> getOffset
  <*> get
  <*> wordSenseStatement `NC.endBy1` linebreak
  <*> definitionStatement <* linebreak
  <*> exampleStatement `endBy` linebreak
  <*> framesStatement <* linebreak
  <*> synsetRelationStatement `NC.endBy1` linebreak

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

--- >>> parseTest definitionStatement "d: oi amigos\n"

exampleStatement :: Parser Text
exampleStatement = statement "e" textBlock

synsetRelationStatement :: Parser SynsetRelation
synsetRelationStatement = L.nonIndented spaceConsumer go
  where
    go = SynsetRelation <$> relationName <*> wordSenseIdentifier
    relationName = T.stripEnd
      -- [ ] handle this better
      <$> takeWhile1P (Just "Synset relation name") (`notElem` [':', ' ', '\n'])
      <* symbol ":"

wordSenseStatement :: Parser WNWord
wordSenseStatement = statement "w" go
  where
    go = WNWord <$> wordSenseIdentifier <*> wordSenseFrames <*> wordSensePointers

wordSenseIdentifier :: Parser (Text, Text, Int)
wordSenseIdentifier = (,,) <$>
  (lexicographerIdentifier <|> get) <*> word <*> lexicalIdentifier
  where
    lexicographerFilePos :: Parser Text
    lexicographerFilePos = choice $
      map string ["noun", "verb", "adjs", "adj", "adv"]
    lexicographerFileName = takeWhile1P Nothing (/= ':')
    lexicographerIdentifier :: Parser Text
    lexicographerIdentifier = do
      pos <- lexicographerFilePos
      _ <- string "."
      fileName <- lexicographerFileName
      _ <- string ":"
      return $ T.concat [pos,".",fileName]

-- >>> parseTest wordSense "artifact 2"
-- ("artifact",2)

wordSensePointers :: Parser [WordPointer]
wordSensePointers = many go
  where
    go = WordPointer <$> (word <?> "Word pointer") <*> wordSenseIdentifier

wordSenseFrames :: Parser [Int]
wordSenseFrames = option [] $ symbol "frames" *> frameNumbers

word :: Parser Text
word = lexeme $ takeWhile1P Nothing (not . isSpace)

lexicalIdentifier :: Parser Int
lexicalIdentifier = option 0 (integer <?> "Lexical Identifier")

framesStatement :: Parser [Int]
framesStatement = statement "fs" frameNumbers

frameNumbers :: Parser [Int]
frameNumbers = some (integer <?> "Frame number")
-- >>> parseTest framesStatement "frame: 2 3"
-- [2,3]

whiteSpaceConsumer :: Parser ()
whiteSpaceConsumer = L.space (void spaceChar) empty empty

lineText :: Parser Text
lineText = T.stripEnd <$> takeWhileP Nothing (/= '\n')

textBlock :: Parser Text
textBlock = T.unwords <$> lineText `sepBy1` try (eol *> some (char ' '))

-- >>> parseTest textBlock "foo\n bar"
-- "foo bar"

linebreak :: Parser ()
linebreak = void $ lexeme eol

linebreaks :: Parser ()
linebreaks = void $ some linebreak

--- >>> parseTest synset "w: A-line\nhyper: woman's_clothing\nd: women's clothing that has a fitted top and a flared skirt that is widest at the hemline;\n   it is called the A-line because the effect resembles the capital letter A"

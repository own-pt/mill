{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Applicative hiding (some,many)
import Control.Monad
import Data.Char
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Megaparsec.Debug

---
---


type WordSenseIdentifier = (Maybe Text, Text, Int)
type SynsetIdentifier = (Maybe Text, Text, Int)
type PointerName = Text
type RelationName = Text
data WordPointer = WordRelation PointerName WordSenseIdentifier
  deriving (Show,Eq)
data SynsetRelation = SynsetRelation RelationName SynsetIdentifier
  deriving (Show,Eq)
type FrameIdentifier = Int
data WNWord = WNWord WordSenseIdentifier [FrameIdentifier] [WordPointer]
  deriving (Show,Eq)

type Parser = Parsec Void Text
data SynsetStatement
  = WordSenseStatement WNWord
  | DefinitionStatement Text
  | ExampleStatement Text
  | FramesStatement [Int]
  | SynsetRelationStatement SynsetRelation
  deriving (Show,Eq)

type RawSynsetStatement = Either (ParseError Text Void) SynsetStatement

lexicographerFile :: Parser (Text, Int, [[RawSynsetStatement]])
lexicographerFile = (,,) <$>
  (word <?> "Lexicographer file name") <*>
  (integer <?> "Lexicographer file identifier") <*>
  synsets

synsets :: Parser [[RawSynsetStatement]]
synsets = synset `sepBy1` consecutiveNewlines

synset :: Parser [RawSynsetStatement]
synset = some synsetStatementOrError
  where
    synsetStatementOrError = withRecovery recover (Right <$> synsetStatement)
    recover :: ParseError Text Void -> Parser RawSynsetStatement
    recover err = Left err <$ skipManyTill anySingle (try $ eol *> notFollowedBy (single ' '))

synsetStatement :: Parser SynsetStatement
synsetStatement = lexeme $
  WordSenseStatement       <$> wordSenseStatement      <|>
  DefinitionStatement      <$> definitionStatement     <|>
  ExampleStatement         <$> exampleStatement        <|>
  FramesStatement          <$> framesStatement         <|>
  SynsetRelationStatement  <$> synsetRelationStatement

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
    go = symbol name *> symbol ":" *> parser <* eol

definitionStatement :: Parser Text
definitionStatement = statement "d" textBlock

--- >>> parseTest definitionStatement "d: oi amigos\n"

exampleStatement :: Parser Text
exampleStatement = statement "e" textBlock

synsetRelationStatement :: Parser SynsetRelation
synsetRelationStatement = L.nonIndented spaceConsumer go
  where
    go = SynsetRelation <$> relationName <*> wordSenseIdentifier
    relationName = T.stripEnd <$> takeWhile1P (Just "Synset relation name") (/= ':')
                              <* symbol ":"

wordSenseStatement :: Parser WNWord
wordSenseStatement = statement "w" go
  where
    go = WNWord <$> wordSenseIdentifier <*> wordSenseFrames <*> wordSensePointers

wordSenseIdentifier :: Parser (Maybe Text, Text, Int)
wordSenseIdentifier = (,,) <$>
  optional lexicographerIdentifier <*> word <*> lexicalIdentifier
  where
    lexicographerFilePos :: Parser Text
    lexicographerFilePos = choice $
      map (try . string) ["noun", "verb", "adj", "adjs", "adv"]
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
    go = WordRelation <$> (word <?> "Word pointer") <*> wordSenseIdentifier

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

line :: Parser Text
line = T.stripEnd <$> takeWhileP Nothing (/= '\n')

textBlock :: Parser Text
textBlock = T.unwords <$> line `sepBy1` try (eol *> space1)

-- >>> parseTest textBlock "foo\n bar"
-- "foo bar"

consecutiveNewlines :: Parser ()
consecutiveNewlines = void $ lexeme eol *> some (lexeme eol)

--- >>> parseTest synset "w: A-line\nhyper: woman's_clothing\nd: women's clothing that has a fitted top and a flared skirt that is widest at the hemline;\n   it is called the A-line because the effect resembles the capital letter A"

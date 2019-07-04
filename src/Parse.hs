{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parse where

import Control.Applicative hiding (some,many)
import Control.Monad.State.Strict
import Data.Char
import Data.Either
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Void (Void)
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

data SynsetStatement
  = WordSenseStatement WNWord
  | DefinitionStatement Text
  | ExampleStatement Text
  | FramesStatement [Int]
  | SynsetRelationStatement SynsetRelation
  deriving (Show,Eq)

type RawSynsetStatement = Either (ParseError Text Void) SynsetStatement
type RawSynset = (Int, [RawSynsetStatement])
---


-- State stores in which lexicographer file we're in, this is useful
-- to fill in implicit references
type Parser = ParsecT Void Text (State Text)

parseLexicographer :: String -> Text
  -> Either String (Text, Int, [(Int, [SynsetStatement])])
parseLexicographer fileName inputText =
  case evalState (runParserT lexicographerFile fileName inputText) "all.all" of
    Right (lexicographerFileName, lexicographerIdentifier, rawSynsets)
      -> let (parseErrors, synsetStatementsWithOffsets) = partitionEithers $ map findParseErrors rawSynsets
         in if null parseErrors
            then Right (lexicographerFileName, lexicographerIdentifier, synsetStatementsWithOffsets)
            else Left $ unlines parseErrors
    Left errors -> Left $ errorBundlePretty errors
  where
    findParseErrors :: RawSynset -> Either String (Int, [SynsetStatement])
    findParseErrors (offset, rawSynsetStmts) = bimap id (offset,)
      $ go [] [] rawSynsetStmts
    go :: [ParseError Text Void] -> [SynsetStatement] -> [RawSynsetStatement] -> Either String [SynsetStatement]
    go [] synsetStatements [] = Right synsetStatements
    go parseErrors _ [] = Left $ concatMap parseErrorPretty $ reverse parseErrors
    go parseErrors synsetStatements (Left parseError:rawSynsetStatements) =
      go (parseError:parseErrors) synsetStatements rawSynsetStatements
    go parseErrors synsetStatements (Right synsetStmt:rawSynsetStatements) =
      go parseErrors (synsetStmt:synsetStatements) rawSynsetStatements


lexicographerFile :: Parser (Text, Int, [RawSynset])
lexicographerFile = do
  _ <- spaceConsumer
  lexicographerFileName <- word <?> "Lexicographer file name"
  lexicographerFileNumber <- integer <?> "Lexicographer file identifier"
  put lexicographerFileName
  _ <- linebreaks
  synsetsStatements <- synsets
  _ <- eof
  return (lexicographerFileName, lexicographerFileNumber, synsetsStatements)

synsets :: Parser [RawSynset]
synsets = synset `sepEndBy1` many linebreak

synset :: Parser RawSynset
synset = (,) <$> getOffset <*> someTill (synsetStatementOrError <* linebreak) linebreak

synsetStatementOrError :: Parser RawSynsetStatement
synsetStatementOrError = withRecovery recover (Right <$> synsetStatement)
  where
    recover :: ParseError Text Void -> Parser RawSynsetStatement
    recover err = Left err <$ textBlock

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

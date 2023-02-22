{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import Data.Maybe
import Control.Monad
import Data.Functor.Identity
import Data.Text (Text)
import Data.Void
import Prelude hiding (mod)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

data Comment = Comment {startLine :: Int, comm :: Text}
  deriving Show

consumeAll :: Parser Comment
consumeAll = skipManyTill anySingle (lineComment <|> blockComment <|> (Comment (-1) "" <$ eof))

lineComment :: Parser Comment
lineComment = do
  pos <- unPos . sourceLine <$> getSourcePos
  Comment pos . T.pack <$> (string "--" *> manyTill anySingle newline)

blockComment :: Parser Comment
blockComment = do
  pos <- unPos . sourceLine <$> getSourcePos
  string "{-" *> (Comment pos . T.pack <$> manyTill anySingle (string "-}")) <* sc

parseComments :: Parser [Comment]
parseComments = manyTill consumeAll eof

data Note = Note { name :: Text -- ^ Name of this note
                 , mod :: Text -- ^ TODO: Delete Module this note is in
                 , body :: [Text] -- ^ The body of the note
                 , references :: [Text] -- ^ Other notes this note mentions
                 } deriving Show

type Parser = ParsecT Void Text Identity

sc :: Parser ()
sc = L.space hspace1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

noteParser :: Text -- ^ Module name
           -> Parser (Maybe Note)
noteParser modName = do
  skipManyTill anySingle (lookAhead (void noteTitle) <|> eof)
  optional $ do
    noteName <- noteTitle
    noteLines <- manyTill noteLine (lookAhead (void noteTitle) <|> eof)
    let refs = case parse (skipManyTill anySingle (lookAhead (void noteReference) <|> eof) *> manyTill (noteReference <* skipManyTill anySingle (lookAhead (void noteReference) <|> eof)) eof) "Parsing references" (T.unwords noteLines) of
                            Left e -> error $ errorBundlePretty e
                            Right r -> r
    pure $ Note {name = noteName, mod=modName, body=noteLines, references=refs}

noteLine :: Parser Text
noteLine = T.pack <$> manyTill anySingle newline

noteTitle :: Parser Text
noteTitle = T.pack <$> try (symbol "Note" *> (symbol "[" *> manyTill anySingle (char ']')) <* sc <* newline <* some (char '~') <* newline)

noteReference :: Parser Text
noteReference = T.pack <$> try (symbol "Note" *> (symbol "[" *> manyTill anySingle (char ']')))

extractNotes :: Text -- ^ Module Name the comment is in
             -> Comment -- ^ Comment
             -> [Maybe Note]
extractNotes modName (Comment _ txt)
  = case parse (manyTill (noteParser modName) eof) "Extract Note" txt of
      Left e -> error (errorBundlePretty e)
      Right ns -> ns

-- | Join multiple single-line comments in consecutive lines into a single multiline comment
joinMultiComments :: [Comment] -> [Comment]
joinMultiComments = foldr (\(Comment startLine s) acc ->
      case acc of
        [] -> [Comment startLine s]
        (Comment n s'):otherComments
          | startLine+1 == n -> Comment startLine (s <> "\n" <> s'):otherComments
          | otherwise -> Comment startLine (s <> "\n"):acc -- The parser expects all comments to end with a newline
    ) mempty

notesInModule :: FilePath -> IO [Note]
notesInModule fp = do
  content <- T.readFile fp
  case parse parseComments fp content of
    Right (joinMultiComments -> comments) ->
      pure $ catMaybes $ concatMap (extractNotes $ T.pack fp) comments
    Left e -> fail ("When parsing " <> fp <> " got \"" <> show e <> "\"")


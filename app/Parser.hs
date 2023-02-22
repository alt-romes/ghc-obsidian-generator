{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Monad
import Data.Functor.Identity
import Data.Text (Text)
import Data.Void
import Prelude hiding (mod)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- We only care about comments
data Module = Module { modpath  :: Text
                     , comments :: [Comment]
                     } deriving Show

ignoreAll :: Parser ()
ignoreAll = L.space
  (void $ notFollowedBy (lineComment <|> blockComment) <* anySingle)
  empty
  empty

lineComment :: Parser Comment
lineComment = Comment <$> (string "--" *> takeWhileP (Just "character") (/= '\n'))

blockComment :: Parser Comment
blockComment = string "{-" *> (Comment . T.pack <$> manyTill anySingle (string "-}")) <* sc

moduleParser :: Text -- ^ Module name
             -> Parser Module
moduleParser modName = do
  comments <- manyTill (ignoreAll *> (blockComment <|> lineComment)) eof
  pure Module{modpath = modName, comments=comments}


-- | Multiline comment
newtype Comment = Comment { comm :: Text }
  deriving Show

data Note = Note { name :: Text -- ^ Name of this note
                 , mod :: Text -- ^ Module this note is in
                 , body :: [Text] -- ^ The body of the note
                 } deriving Show

type Parser = ParsecT Void Text Identity

sc :: Parser ()
sc = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

noteParser :: Text -- ^ Module name
           -> Parser Note
noteParser modName = do
  noteName <- noteTitle
  noteLines <- manyTill noteLine (lookAhead (void noteTitle) <|> eof)
  pure $ Note {name = noteName, mod=modName, body=noteLines}

noteLine :: Parser Text
noteLine = T.pack <$> (manyTill printChar newline <* sc)

noteTitle :: Parser Text
noteTitle = T.pack <$> (symbol "Note" *> lexeme (symbol "[" *> manyTill printChar (char ']')) <* some (char '~'))

extractComments :: Text -- ^ Module name
                -> Text -- ^ Content
                -> Module
extractComments modName txt
  = case parse (moduleParser modName) "Extract module" txt of
      Left e -> error (show e)
      Right ns -> ns

extractNotes :: Text -- ^ Module Name the comment is in
             -> Comment -- ^ Comment
             -> [Note]
extractNotes modName (Comment txt)
  = case parse (sc *> many (noteParser modName)) "Extract Note" txt of
      Left e -> error (show e)
      Right ns -> ns



{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module NewParser where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Applicative (liftA2)
import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Text (Text)
import Data.Void

type Parser = ParsecT Void Text Identity


data Note = Note { title      :: NoteTitle -- ^ Name of this note
                 , body       :: [Text] -- ^ The body of the note
                 , references :: [NoteReference] -- ^ Other notes this note mentions
                 , subnotes   :: [Note]
                 } deriving Show

data NoteTitle = NoteTitle Text Bool -- ^ Is historic note?
  deriving Show

newtype NoteReference = NoteReference Text deriving Show

parseCNote :: Parser Note
parseCNote = noteParser id (string "*/")

parseHaskellNote :: Parser Note
parseHaskellNote = lineNote <|> blockNote
  where
    lineNote = noteParser (string "--" *>) (notFollowedBy (string "--"))
    blockNote = noteParser id (string "-}") -- TODO: Doesn't end at pragma end

-- Assumes no previous input, much like how whitespace lexing works
noteParser :: (∀ a. Parser a -> Parser a) -> Parser end -> Parser Note
noteParser prefixf end = do
  noteName  <- noteTitle prefixf
  noteLines <- manyTill (prefixf noteLine) (void end <|> lookAheadNoteTitle)
  skipAnyTill (lookAheadNoteTitle)
  let refs = case parse parseReferences "References" (T.unwords noteLines) of
        Left e -> error $ errorBundlePretty e
        Right r -> r
  pure $ Note {title = noteName, body=noteLines, references=refs, subnotes=[]}
    where
      lookAheadNoteTitle = lookAhead (void $ noteTitle id) <|> eof

noteTitle :: (∀ a. Parser a -> Parser a) -> Parser NoteTitle
noteTitle prefixf = try $ do
  historic <- prefixf $ optional (symbol "Historic")
  ntitle   <- symbol "Note" *> (string "[" *> manyTill anySingle (string "]")) <* eol
                <* prefixf (hspace <* some (char '~') <* eol)
  pure $ NoteTitle (T.pack ntitle) (isJust historic)

parseReferences :: Parser [NoteReference]
parseReferences = skipAnyTill lookAheadNoteRef *> manyTill (noteReference <* skipAnyTill lookAheadNoteRef) eof
  where
      lookAheadNoteRef = lookAhead (void noteReference) <|> eof

noteReference :: Parser NoteReference
noteReference = NoteReference . T.pack <$> try (symbol "Note" *> (string "[" *> manyTill anySingle (string "]")))

noteLine :: Parser Text
noteLine = T.pack <$> manyTill anySingle (void eol <|> eof)

skipAnyTill :: Parser end -> Parser end
skipAnyTill = skipManyTill anySingle

symbol :: Text -> Parser Text
symbol s = string' s <* hspace

notesInModule :: FilePath -> IO [Note]
notesInModule fp = do
  -- TODO: If ends with... c call, hs call ...
  content <- T.readFile fp
  let topParser = skipAnyTill (eof <|> void (lookAhead parseHaskellNote)) *> manyTill parseHaskellNote eof
  case parse topParser fp content of
    Right ns -> pure ns
    Left e -> fail ("When parsing " <> fp <> " got \"" <> errorBundlePretty e <> "\"")

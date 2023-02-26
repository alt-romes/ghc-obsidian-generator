{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
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

data NoteType = LineNote | BlockNote

-- parseCNote :: Parser Note
-- parseCNote = noteParser id (string "*/")

parseHaskellNote :: Parser Note
parseHaskellNote = noteParser LineNote <|> noteParser BlockNote
  -- where
  --   lineNote = noteParser (hspace *> string "--" *>) (notFollowedBy (string "--" *> hspace))
  --   blockNote = noteParser id (string "-}") -- TODO: Doesn't end at pragma end


-- Assumes no previous input, much like how whitespace lexing works
noteParser :: NoteType -> Parser Note
noteParser nt = do
  noteName  <- noteTitle nt
  noteLines <- manyTill (noteLine nt) (endComment nt <|> lookAheadNoteTitle)
  skipLinesTill lookAheadNoteTitle
  let refs = case parse parseReferences "References" (T.unwords noteLines) of
        Left e -> error $ errorBundlePretty e
        Right r -> r
  pure $ Note {title = noteName, body=noteLines, references=refs, subnotes=[]}
    where
      lookAheadNoteTitle = lookAhead (void $ noteTitle nt) <|> eof

noteTitle :: NoteType -> Parser NoteTitle
noteTitle nt = try $ do
  (historic, ntitle) <- case nt of
    LineNote -> do
      historic <- hsc *> symbol "--" *> pHistoric
      ntitle   <- pTitle
                    <* hsc <* symbol "--" <* pTilde
      pure (historic, ntitle)
    BlockNote -> do
      historic <- optional (symbol "{-") *> pHistoric -- optional {- to account for notes starting on the comment block
      ntitle   <- hsc *> pTitle <* pTilde
      pure (historic, ntitle)
  pure $ NoteTitle (T.pack ntitle) (isJust historic)
  where
    -- there exist both "Historic" and "Historical" Notes. Same meaning, different syntax
    pHistoric = optional (string "Historic" *> string "al" *> hsc)
    pTitle = symbol "Note" *> string "[" *> manyTill anySingle (string "]") <* skipAnyTill eol
    pTilde = some (char '~') <* eol

endComment :: NoteType -> Parser ()
endComment = \case
  LineNote -> notFollowedBy (string "--")
  BlockNote -> void $ string "-}"

parseReferences :: Parser [NoteReference]
parseReferences = skipAnyTill lookAheadNoteRef *> manyTill (noteReference <* skipAnyTill lookAheadNoteRef) eof
  where lookAheadNoteRef = lookAhead (void noteReference) <|> eof

noteReference :: Parser NoteReference
noteReference = NoteReference . T.pack . unwords <$> try (symbol "Note" *> (string "[" *> manyTill (manyTill anySingle (space1 <|> void (lookAhead (char ']')))) (string "]")))

noteLine :: NoteType -> Parser Text
noteLine nt = fmap T.pack $ case nt of
  LineNote  -> symbol "--" *> manyTill anySingle (void eol <|> eof)
  BlockNote -> manyTill anySingle (lookAhead (void $ string "-}") <|> void eol <|> eof)


skipAnyTill :: Parser end -> Parser end
skipAnyTill = skipManyTill anySingle

skipLinesTill :: Parser end -> Parser end
skipLinesTill = skipManyTill (skipAnyTill (void eol <|> eof))

hsc :: Parser ()
hsc = skipMany (hidden hspace1)

symbol :: Text -> Parser Text
symbol s = string' s <* hsc

notesInModule :: FilePath -> IO [Note]
notesInModule fp = do
  -- TODO: If ends with... c call, hs call ...
  content <- T.readFile fp
  let topParser = skipLinesTill (eof <|> void (lookAhead parseHaskellNote)) *> manyTill parseHaskellNote eof
  case parse topParser fp content of
    Right ns -> pure ns
    Left e -> fail ("When parsing " <> fp <> " got \"" <> errorBundlePretty e <> "\"")

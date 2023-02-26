{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.FilePath
import Data.Text (Text)
import Documentation.Haddock -- TODO: Also export documentation with function signatures (and possibly the definition in a spoiler tag)
import System.Directory
import System.FilePattern.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T

import NewParser

class Pretty p where
  ppr :: p -> Text

instance Pretty Note where
  ppr (Note{title, body, references}) =
     (case references of
           [] -> ""
           (NoteReference x:xs) -> "\nReferences:" <> foldr (\(NoteReference y) -> ((" [[" <> normalizeNoteName y <> "]],") <> )) (" [[" <> normalizeNoteName x <> "]]") xs <> "\n\n"
        ) <> "```\n" <> ppr title <> T.unlines body <> "```\n"

instance Pretty NoteTitle where
  -- We only pretty print note title in the body of the note, so we don't worry about the name being escaped
  ppr (NoteTitle t h) = t' <> "\n" <> tildes <> "\n"
    where
      t' = (if h then ("Historical " <>) else id) "Note [" <> t <> "]"
      tildes = T.pack $ map (const '~') [1..T.length t']

normalizeNoteName :: Text -> Text
normalizeNoteName = T.replace ":" ";" . T.replace "#" "H" . T.replace "/" "Y"
                  -- # and / and : are forbidden in Obsidian links, so we work around it

{- 
Note [Teste de som]
~~~~~~~~~~~~~~~~~~~

Here's more on this note...
-}

-- Note [Another kind of note]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
-- More more moremore things
-- Yes things no,... oK.

-- Note [Double of note]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Content 1
-- Content 1.1
-- 
-- Note [Double note 2]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 
-- Content 1
-- Content 1.2
-- Yes things no,... oK.

-- | Main comment of main function
-- Double line!
main :: IO ()
main = do
  -- readFile "app/Main.hs" >>= 
  let notes_dir = "NotesVault"
  hscs <- doesDirectoryExist "compiler" >>= \case
    False -> couldn't_find "compiler"
    True  -> map ("compiler" </>) <$> getDirectoryFiles "compiler" ["**" </> "*.hs"]
  libs <- doesDirectoryExist "libraries" >>= \case
    False -> couldn't_find "libraries"
    True  -> map ("libraries" </>) <$> getDirectoryFiles "libraries" ["**" </> "*.hs"]
  -- cs <- doesDirectoryExist "rts" >>= \case
  --   False -> couldn't_find "rts"
  --   True  -> map ("rts" </>) <$> getDirectoryFiles "rts" ["**" </> "*.c", "**" </> "*.cmm", "**" </> "*.h"]
  mapM_ (\f -> do
    putStrLn f
    let output_dir = notes_dir </> f
    notes <- notesInModule f
    createDirectoryIfMissing True output_dir
    mapM_ (\n -> T.writeFile (output_dir </> T.unpack ((\(NoteTitle t _) -> normalizeNoteName t) (title n)) <.> "md") (ppr n)) notes
        ) (hscs <> libs)
  where
    couldn't_find dir = fail $ "Couldn't find the directory '"<> dir <>"'. This program expects to be run in the root of the ghc tree."




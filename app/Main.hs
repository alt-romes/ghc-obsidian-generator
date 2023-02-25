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

import Parser

ppr :: Note -> Text
ppr (Note{name, body, references}) =
  let title = "Note [[" <> name <> "]]"
      tildes = T.pack $ map (const '~') [1..T.length title]
   in (case references of
         [] -> ""
         (x:xs) -> "\nReferences:" <> foldr (\y -> ((" [[" <> normalizeNoteName y <> "]],") <> )) (" [[" <> normalizeNoteName x <> "]]") xs <> "\n\n"
      ) <> "```\n" <> title <> "\n" <> tildes <> "\n\n" <> T.unlines body <> "```\n"

normalizeNoteName :: Text -> Text
normalizeNoteName = T.replace "#" "H" . T.replace "/" "|" -- # and / are forbidden in Obsidian links, so we work around it

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
  doesDirectoryExist "compiler" >>= \case
    False -> putStrLn "Couldn't find the directory 'compiler'. This program expects to be run in the root of the ghc tree."
    True  -> do
      hscs <- getDirectoryFiles "compiler" ["**" </> "*.hs"]
      mapM_ (\(("compiler" </>) -> hsf) -> do
        print hsf
        let output_dir = "NotesVault" </> hsf
        notes <- notesInModule hsf
        createDirectoryIfMissing True output_dir
        mapM_ (\n -> T.writeFile (output_dir </> T.unpack (normalizeNoteName $ name n) <.> "md") (ppr n)) notes
            ) hscs




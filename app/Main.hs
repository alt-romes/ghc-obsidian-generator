{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Data.Text (Text)
import Documentation.Haddock -- TODO: Also export documentation with function signatures (and possibly the definition in a spoiler tag)
import System.Directory
import System.FilePattern.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Exts as Ext

import Parser

-- | Join multiple single-line comments in consecutive lines into a single multiline comment
joinMultiComments :: [Ext.Comment] -> [Comment]
joinMultiComments = map snd . go
  where
    go :: [Ext.Comment] -> [(Int, Comment)] -- ^ (StartLine, Comment)
    go = foldr (\(Ext.Comment _ (Ext.srcSpanStart -> (startLine,_)) (T.pack -> s)) acc ->
        case acc of
          [] -> [(startLine, Comment s)]
          (n,Comment s'):otherComments
            | startLine+1 == n -> (startLine, Comment $ s <> "\n" <> s'):otherComments
            | otherwise -> (startLine, Comment $ s <> "\n"):acc -- The parser expects all comments to end with a newline
      ) mempty

getModName :: Ext.Module w -> Text
getModName (Ext.Module _ Nothing _ _ _) = "Main"
getModName (Ext.Module _ (Just (Ext.ModuleHead _ (Ext.ModuleName _ str) _ _)) _ _ _) = T.pack str
getModName _ = error "internal error"

notesInModule :: FilePath -> IO [Note]
notesInModule fp = do
  Ext.parseFileWithComments (simpleParseMode fp) fp >>= \case
    Ext.ParseOk (getModName -> modName,joinMultiComments -> comments) ->
      pure $ concatMap (extractNotes modName) comments
    Ext.ParseFailed _ e -> fail ("When parsing " <> fp <> " got \"" <> e <> "\"")
  where
    simpleParseMode name = Ext.ParseMode name Ext.Haskell2010 [Ext.EnableExtension x | x <- [minBound..maxBound]] True True Nothing True


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
  doesDirectoryExist "compiler" >>= \case
    False -> putStrLn "Couldn't find the directory 'compiler'. This program expects to be run in the root of the ghc tree."
    True  -> do
      hscs <- getDirectoryFiles "compiler" ["**/*.hs"]
      mapM_ ((mapM_ print <=< notesInModule) . ("compiler/" <>)) hscs



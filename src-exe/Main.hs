module Main where

import Data.Bifunctor (first)
import Data.Text as T
import Data.Text.IO
import Prelude hiding (readFile, putStrLn)
import Control.Monad (zipWithM_)

import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Data.Text.Prettyprint.Doc (pretty)

import System.Environment (getArgs)

import AbstractSyntaxTree
import Parser (parseProgram)
import TypeChecker (checkProgram)

main :: IO ()
main = do
  files <- getArgs
  programs <- mapM readFile files
  zipWithM_ (\file program ->
    either putStrLn
           (mapM_ print)
           (first (T.pack . errorBundlePretty)
                  (parse parseProgram file program)
            >>= checkProgram))
            files
            programs

module Main where

import Data.Bifunctor (first)
import qualified Data.Text as T
import Data.Text.IO
import Prelude hiding (readFile, putStrLn)
import Control.Monad (zipWithM_)

import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc

import System.Environment (getArgs)

import AbstractSyntaxTree
import Parser (parseProgram)
import TypeChecker (checkProgram)

main :: IO ()
main = do
  files <- getArgs
  programs <- mapM readFile files
  zipWithM_ (\file program ->
    either putDoc
           (putDoc . vsep . map pretty)
           (first (pretty . errorBundlePretty)
                  (parse parseProgram file program)
            >>= checkProgram))
            files
            programs

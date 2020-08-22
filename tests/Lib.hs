module Lib where

import Test.Hspec
import Test.Hspec.Megaparsec

import Control.Monad.Except
import Control.Monad.State
import qualified Control.Monad.State.Strict as Strict

import AbstractSyntaxTree
import Parser
import TypeChecker

import Data.Text
import Data.Bifunctor (first)

import Text.Megaparsec

import Lens.Micro.Platform

import Prelude hiding (unlines)

-- | first parses definitions then checks if parsing the expression
--   matches the expected expression in the parsed context
shouldParseWithDefs :: [Text] -- ^ definitions to parse
                    -> Text -- ^ expression to parse
                    -> [Statement] -- ^ expected parsing result
                    -> Expectation
shouldParseWithDefs defs input expOutput =
  case parse (Strict.execStateT (many parseStatement <* eof) emptyState)
             "" (unlines defs) of
    Left err -> error . show $ errorBundlePretty err
    Right pState ->
      parse (Strict.evalStateT (many parseStatement <* eof) pState)
            "" input
      `shouldParse`
      expOutput

shouldCheckIn :: (HasCallStack, Show a, Eq a)
              => TI ann a
              -> ContextTI
              -> a
              -> Expectation
shouldCheckIn ti ctx' val = first show (runTI ti ctx')
                            `shouldBe`
                            Right val

shouldCheckInGlobCtx :: (HasCallStack, Show a, Eq a)
                   => [Statement]
                   -> TI ann a
                   -> a
                   -> Expectation
shouldCheckInGlobCtx defCtx' ti =
  shouldCheckIn ti (set defCtx defCtx' emptyCtx)


-- | first parses definition then check if type checking the expression
--   matches the expected type in the parsed context
shouldCheckWithDefs :: HasCallStack
                    => [Text] -- ^ definitions to parse
                    -> Expr  -- ^ expression to type check
                    -> Type -- ^ expected type
                    -> Expectation
shouldCheckWithDefs defs input expOutput =
  case parse parseProgram "" (unlines defs) of
    Left err -> error . show $ errorBundlePretty err
    Right defCtx' ->
      let typedDefCtx =
            execState (runExceptT $ checkProgramPTI defCtx') []
      in shouldCheckInGlobCtx typedDefCtx (inferTerm input) expOutput

-- | first parses definition then check if evaluating the expression
--   matches the expected one in the parsed context
shouldEvalWithDefs :: HasCallStack
                   => [Text] -- ^ definitions to parse
                   -> Expr  -- ^ expression to evaluate
                   -> Expr -- ^ expected expression
                   -> Expectation
shouldEvalWithDefs defs input expOutput =
  case parse parseProgram "" (unlines defs) of
    Left err -> error . show $ errorBundlePretty err
    Right defCtx' ->
      let typedDefCtx =
            execState (runExceptT $ checkProgramPTI defCtx') []
      in shouldCheckInGlobCtx typedDefCtx (evalExpr input) expOutput

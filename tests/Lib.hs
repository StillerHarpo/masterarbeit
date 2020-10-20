module Lib where

import qualified Data.Text                  as T
import           Data.Text                                   (Text)
import           Data.Bifunctor                              (first)

import           Control.Monad.Except
import           Control.Monad.State
import qualified Control.Monad.State.Strict as Strict

import           Text.Megaparsec

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Test.Hspec.Hedgehog                         ( (===)
                                                             , MonadTest)

import           Lens.Micro.Platform

import           AbstractSyntaxTree
import           Parser
import           TypeChecker
import           TypeAction
import           Eval

-- | first parses definitions then checks if parsing the expression
--   matches the expected expression in the parsed context
shouldParseWithDefs :: HasCallStack
                    => [Text] -- ^ definitions to parse
                    -> Text -- ^ expression to parse
                    -> [Decl] -- ^ expected parsing result
                    -> Expectation
shouldParseWithDefs defs input expOutput =
  case parse (Strict.execStateT (many parseDecl <* eof) emptyState)
             "" (T.unlines defs) of
    Left err     ->
      error . show $ errorBundlePretty err
    Right pState ->
      parse (Strict.evalStateT (many parseDecl <* eof) pState)
            "" input
      `shouldParse`
      expOutput

shouldEvalIn' :: (HasCallStack, Show a, Eq a)
              => (Either String a -> Either String a -> m ())
              -> Eval ann a
              -> EvalCtx
              -> a
              -> m ()
shouldEvalIn' comp eval ctx' val = first show (runEval eval ctx')
                                   `comp`
                                   Right val

shouldEvalIn :: (HasCallStack, Show a, Eq a)
              => Eval ann a
              -> EvalCtx
              -> a
              -> Expectation
shouldEvalIn = shouldEvalIn' shouldBe

shouldEvalInGlobCtx' :: (HasCallStack, Show a, Eq a)
                   => (Either String a -> Either String a -> m ())
                   -> [Decl]
                   -> Eval ann a
                   -> a
                   -> m ()
shouldEvalInGlobCtx' comp declCtx' eval =
  shouldEvalIn' comp eval (emptyEvalCtx {declCtx = declCtx'})

shouldEvalInGlobCtx :: (HasCallStack, Show a, Eq a)
                    => [Decl]
                    -> Eval ann a
                    -> a
                    -> Expectation
shouldEvalInGlobCtx = shouldEvalInGlobCtx' shouldBe


shouldCheckIn' :: (HasCallStack, Show a, Eq a)
              => (Either String a -> Either String a -> m ())
              -> TI ann a
              -> ContextTI
              -> a
              -> m ()
shouldCheckIn' comp ti ctx' val = first show (runTI ti ctx')
                                  `comp`
                                  Right val

shouldCheckIn :: (HasCallStack, Show a, Eq a)
              => TI ann a
              -> ContextTI
              -> a
              -> Expectation
shouldCheckIn = shouldCheckIn' shouldBe

shouldCheckInGlobCtx' :: (HasCallStack, Show a, Eq a)
                   => (Either String a -> Either String a -> m ())
                   -> [Decl]
                   -> TI ann a
                   -> a
                   -> m ()
shouldCheckInGlobCtx' comp defCtx' ti =
  shouldCheckIn' comp ti (set defCtx defCtx' emptyCtx)

shouldCheckInGlobCtx :: (HasCallStack, Show a, Eq a)
                   => [Decl]
                   -> TI ann a
                   -> a
                   -> Expectation
shouldCheckInGlobCtx = shouldCheckInGlobCtx' shouldBe

shouldRunWithDefs' :: (HasCallStack, Show a, Eq a, MonadIO m)
                   => (Either String a -> Either String a -> m ())
                   -> [Text]
                   -> TI ann a
                   -> a
                   -> m ()
shouldRunWithDefs' comp defs action expOutput =
  case parse parseProgram "" (T.unlines defs) of
    Left err      ->
      liftIO $ error . show $ errorBundlePretty err
    Right defCtx' ->
      let typedDefCtx =
            execState (runExceptT $ checkProgramPTI defCtx') []
      in shouldCheckInGlobCtx' comp typedDefCtx action expOutput

-- | first parses definition then checks if evaluation of action
--   matches the expected output in the parsed context
shouldRunWithDefs :: (HasCallStack, Show a, Eq a)
                  => [Text] -- ^ definitions to parse
                  -> TI ann a  -- ^ action to evaluate
                  -> a -- ^ expected output
                  -> Expectation
shouldRunWithDefs = shouldRunWithDefs' shouldBe


shouldCheckWithDefs' :: (HasCallStack, MonadIO m)
                     => (Either String Type -> Either String Type -> m ())
                     -> [Text]
                     -> Expr
                     -> Type
                     -> m ()
shouldCheckWithDefs' comp defs input =
  shouldRunWithDefs' comp defs (inferTerm input)

-- | first parses definition then check if type checking the expression
--   matches the expected type in the parsed context
shouldCheckWithDefs :: HasCallStack
                    => [Text] -- ^ definitions to parse
                    -> Expr  -- ^ expression to type check
                    -> Type -- ^ expected type
                    -> Expectation
shouldCheckWithDefs = shouldCheckWithDefs' shouldBe

-- | first parses definition then check if type checking the expression
--   matches the expected type in the parsed context
--   (version for Hedgehog)
shouldCheckWithDefsP :: (HasCallStack, MonadTest m, MonadIO m)
                     => [Text] -- ^ definitions to parse
                     -> Expr  -- ^ expression to type check
                     -> Type -- ^ expected type
                     -> m ()
shouldCheckWithDefsP = shouldCheckWithDefs' (===)

shouldCheckDeclWithDefs :: HasCallStack
                        => [Text] -- ^ definitions to parse
                        -> Decl  -- ^ Declaration to type check
                        -> Type -- ^ expected type
                        -> Expectation
shouldCheckDeclWithDefs defs decl expTy =
  case parse parseProgram "" (T.unlines defs) of
    Left err     ->
      error . show $ errorBundlePretty err
    Right defCtx' ->
      case runState (runExceptT $ checkProgramPTI
                                $ defCtx' ++ [decl]) [] of
        (Left err, _                      ) ->
          error $ show err
        (_       , ExprDef{ty = Just ty}:_) ->
          ty `shouldBe` expTy
        (_       , decl                 :_) ->
          error $ "First of statements is not a expression \nit ist"
                  <> show decl

shouldKindCheckWithDefs' :: (HasCallStack, MonadIO m)
                     => (Either String Kind -> Either String Kind -> m ())
                     -> [Text]
                     -> TypeExpr
                     -> Kind
                     -> m ()
shouldKindCheckWithDefs' comp defs input =
  shouldRunWithDefs' comp defs (inferType input)

-- | first parses definition then check if type checking the expression
--   matches the expected type in the parsed context
shouldKindCheckWithDefs :: HasCallStack
                        => [Text] -- ^ definitions to parse
                        -> TypeExpr  -- ^ expression to type check
                        -> Kind -- ^ expected type
                        -> Expectation
shouldKindCheckWithDefs = shouldKindCheckWithDefs' shouldBe

shouldEvalWithDefs' :: (HasCallStack, MonadIO m)
                    => (Either String Expr -> Either String Expr -> m ())
                    -> [Text]
                    -> Expr
                    -> Expr
                    -> m ()
shouldEvalWithDefs' comp defs input =
  shouldRunWithDefs' comp defs (evalInTI $ evalExpr input)

-- | first parses definition then check if evaluating the expression
--   matches the expected one in the parsed context
shouldEvalWithDefs :: HasCallStack
                   => [Text] -- ^ definitions to parse
                   -> Expr  -- ^ expression to evaluate
                   -> Expr -- ^ expected expression
                   -> Expectation
shouldEvalWithDefs = shouldEvalWithDefs' shouldBe

-- | first parses definition then check if evaluating the expression
--   matches the expected one in the parsed context
--   (version for Hedgehog)
shouldEvalWithDefsP :: (HasCallStack, MonadTest m, MonadIO m)
                    => [Text] -- ^ definitions to parse
                    -> Expr  -- ^ expression to evaluate
                    -> Expr -- ^ expected expression
                    -> m ()
shouldEvalWithDefsP = shouldEvalWithDefs' (===)

-- | first parses definition then check if evaluating the expression
--   matches the expected one in the parsed context
shouldEvalSameWithDefs :: HasCallStack
                       => [Text] -- ^ definitions to parse
                       -> Expr  -- ^ expression to evaluate
                       -> Expr -- ^ expected expression
                       -> Expectation
shouldEvalSameWithDefs defs input1 input2 =
   case parse parseProgram "" (T.unlines defs) of
    Left err      ->
      liftIO $ error . show $ errorBundlePretty err
    Right defCtx' ->
      let typedDefCtx =
            execState (runExceptT $ checkProgramPTI defCtx') []
          stateEval = EvalCtx { declCtx = typedDefCtx
                              , numPars = 0}
          val1 = first show (runEval (evalExpr input1) stateEval)
          val2 = first show (runEval (evalExpr input2) stateEval)
      in val1 `shouldBe` val2

{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Betaeq where

import Control.Monad.Except

import Data.Text.Prettyprint.Doc

import AbstractSyntaxTree
import Subst
import TypeAction
import Eval

betaeq :: TypeExpr -> TypeExpr -> Eval ann ()
betaeq e1 e2 = do
  ee1 <- inlineTypeExpr e1 >>= evalTypeExpr
  ee2 <- inlineTypeExpr e2 >>= evalTypeExpr
  if ee1 == ee2
  then pure ()
  else throwError $ "couldn't match type"
                  <+> pretty e1
                  <+> "with type"
                  <+> pretty e2

betaeqCtx :: Ctx -> Ctx -> Eval ann ()
betaeqCtx ctx1 ctx2 = do
  assert (length ctx1 == length ctx2)
         ("length of context" <+> pretty ctx1 <>
          "\ndoesn't match length of context" <+> pretty ctx2)
  zipWithM_ betaeq ctx1 ctx2

betaeqTyCtx :: TyCtx -> TyCtx -> Eval ann ()
betaeqTyCtx ctx1 ctx2 = do
  assert (length ctx1 == length ctx2)
         ("length of type context " <+> pretty ctx1 <>
          "\ndoesn't match length of type context" <+> pretty ctx2)
  zipWithM_ betaeqCtx ctx1 ctx2

inlineFuns :: OverFunsM (Eval ann)
inlineFuns = (overFunsM inlineFuns)
               { fTyExprM = inlineTypeExpr
               , fExprM   = inlineExpr }

inlineTypeExpr :: TypeExpr -> Eval ann TypeExpr
inlineTypeExpr (GlobalTypeVar n vars)  = lookupDefTypeExpr n vars
                                         >>= inlineTypeExpr
inlineTypeExpr tyExpr                  = overTypeExprM inlineFuns tyExpr

inlineDuc :: OpenDuctive -> Eval ann OpenDuctive
inlineDuc = overOpenDuctiveM inlineFuns

inlineExpr :: Expr -> Eval ann Expr
inlineExpr (GlobalExprVar n tyPars exprPars) =
  lookupDefExpr n tyPars exprPars >>= inlineExpr
inlineExpr expr                              =
  overExprM inlineFuns expr

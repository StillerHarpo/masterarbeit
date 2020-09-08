{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Eval where

import Control.Monad.Reader

import Lens.Micro.Platform

import AbstractSyntaxTree
import ShiftFreeVars
import Subst
import TypeAction

evalTypeExpr :: TypeExpr -> TI ann TypeExpr
evalTypeExpr (f :@ arg) = do
  valF <- evalTypeExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    (Abstr _ expr,_) -> evalTypeExpr $ substTypeExpr 0 valArg expr
    _ -> pure $ valF :@ valArg
evalTypeExpr e = overTypeExprM evalTypeExpr evalDuctive evalExpr e

evalCtx :: Ctx -> TI ann Ctx
evalCtx = mapM evalTypeExpr

evalType :: Type -> TI ann Type
evalType (ctx', tyExpr) = (,) <$> evalCtx ctx' <*> evalTypeExpr tyExpr

evalDuctive :: Ductive -> TI ann Ductive
evalDuctive = overDuctiveM evalTypeExpr evalExpr evalStrDef

evalStrDef :: StrDef -> TI ann StrDef
evalStrDef = overStrDefM evalTypeExpr evalExpr

evalExpr :: Expr -> TI ann Expr
evalExpr (f :@: arg) = do
  valF <- evalExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    -- TODO Should we check if _ = sigma_k\circ \tau
    (getExprArgs -> (r@Rec{..}, sigmaktau), getExprArgs -> (Constructor _ i, constrArgs))
      | length sigmaktau == length (gamma fromRec) -> do
      let gamma1' = gamma1 $ strDefs fromRec !! i
      recEval <- typeAction (a $ strDefs fromRec !! i)
                            [applyExprArgs (r, idCtx (gamma fromRec))
                            :@: LocalExprVar 0 ""]
                            [gamma1']
                            [In fromRec]
                            [toRec]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1')
                                   (1 + length gamma1')
                                   (substExprs 0 (reverse constrArgs)
                                                 (substExpr 0 recEval
                                                              (matches !! i)))
    (getExprArgs -> (Destructor ductive i, tau) , getExprArgs -> (c@Corec{..}, args))
      | length tau == length (gamma1 $ strDefs toCorec !! i) -> do
      let gamma1' = gamma1 $ strDefs toCorec !! i
      recEval <- typeAction (a $ strDefs toCorec !! i)
                            [applyExprArgs (c, idCtx (gamma toCorec))
                            :@: LocalExprVar 0 ""]
                            [gamma1']
                            [fromCorec]
                            [Coin toCorec]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1')
                                   (1 + length gamma1')
                                   (substExprs 0 (last args : reverse tau)
                                                 (substExpr 0 (matches !! i)
                                                              recEval))
    _ -> pure $ valF :@: valArg
evalExpr (GlobalExprVar v tyPars exprPars) =
  lookupDefExprTI v tyPars exprPars >>= evalExpr
evalExpr (WithParameters pars expr) = evalExpr $ substParsInExpr 0 (reverse pars) expr
evalExpr e = overExprM evalTypeExpr evalDuctive evalExpr e

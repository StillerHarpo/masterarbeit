{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Eval where

import AbstractSyntaxTree
import ShiftFreeVars
import Subst
import TypeAction

evalFuns :: OverFunsM (Eval ann)
evalFuns = (overFunsM evalFuns)
             { fTyExprM = evalTypeExpr
             , fExprM   = evalExpr}

evalTypeExpr :: TypeExpr -> Eval ann TypeExpr
evalTypeExpr (f :@ arg) = do
  valF <- evalTypeExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    (Abstr _ expr,_) -> evalTypeExpr $ substTypeExpr 0 valArg expr
    _ -> pure $ valF :@ valArg
evalTypeExpr e          = overTypeExprM evalFuns e

evalCtx :: Ctx -> Eval ann Ctx
evalCtx = overCtxM evalFuns

evalType :: Type -> Eval ann Type
evalType (ctx', tyExpr) = (,) <$> evalCtx ctx' <*> evalTypeExpr tyExpr

evalExpr :: Expr -> Eval ann Expr
evalExpr (f :@: arg)                       = do
  valF <- evalExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    -- TODO Should we check if _ = sigma_k\circ \tau
    (getExprArgs -> (r@Iter{..}, sigmaktau), getExprArgs -> (Structor{num = i, ductive = duc2}, constrArgs))
      | length sigmaktau == length (gamma ductive)
        && inOrCoin ductive == IsIn
        && inOrCoin duc2 == IsIn -> do
      let StrDef{..} = strDefs ductive !! i
          openDuctive = ductive
          parametersTyExpr = parameters
      recEval <- typeAction (substPars 0 (reverse parameters) a)
                            -- TODO shift idCtx by one
                            [applyExprArgs (r, idCtx (gamma ductive))
                            :@: LocalExprVar 0 False ""]
                            [gamma1]
                            [Ductive{..}]
                            [motive]
      evalExpr $ substExprs 0
                            (reverse constrArgs)
                            (substExpr 0
                                       recEval
                                      (shiftFreeVarsExpr ((-1) - length gamma1)
                                                  (1 + length gamma1)
                                                  (matches !! i)))
    (getExprArgs -> (Structor{num=i, ductive = duc2}, tau) , getExprArgs -> (c@Iter{..}, args))
      | inOrCoin ductive == IsCoin
        && inOrCoin duc2 == IsCoin
        && length tau == length (gamma1 $ strDefs ductive !! i) -> do
      let StrDef{..} = strDefs ductive !! i
          openDuctive = ductive
          parametersTyExpr = parameters
      recEval <- typeAction (substPars 0 (reverse parameters) a)
                            -- TODO shift idCtx by one
                            [applyExprArgs (c, idCtx (gamma ductive))
                            :@: LocalExprVar 0 False ""]
                            [gamma1]
                            [motive]
                            [Ductive{..}]
      evalExpr $ substExprs 0
                            (last args : reverse tau)
                            (substExpr  0
                                        (shiftFreeVarsExpr ((-1) - length gamma1)
                                                           (1 + length gamma1)
                                                           (matches !! i))
                                        recEval)
    _ -> pure $ valF :@: valArg
evalExpr (GlobalExprVar v tyPars exprPars) = do
  valTyPars <- mapM evalTypeExpr tyPars
  valExprPars <- mapM evalExpr exprPars
  lookupDefExpr v valTyPars valExprPars >>= evalExpr
evalExpr e =
  overExprM evalFuns e

evalDuctive :: OpenDuctive -> Eval ann OpenDuctive
evalDuctive = overOpenDuctiveM evalFuns

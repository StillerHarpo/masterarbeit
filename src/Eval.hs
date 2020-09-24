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
    (Abstr _ _ expr,_) -> evalTypeExpr $ substTypeExpr 0 valArg
                                       $ shiftFreeVarsTypeExpr (-1) 1 expr
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
    (getExprArgs -> (r@Iter{..}, sigmaktau), getExprArgs -> (Structor{num = i, ductive = duc2}, constrArgs))
      | length sigmaktau == length (gamma ductive)
        && inOrCoin ductive == IsIn
        && inOrCoin duc2 == IsIn -> do
      let StrDef{..} = strDefs ductive !! i
          openDuctive = ductive
          parametersTyExpr = parameters
      recEval <- typeAction (substPars 0 (reverse parameters) a)
                            1
                            [applyExprArgs (r, map (shiftFreeVarsExpr 1 0)
                                               $ idCtx (gamma ductive))
                            :@: LocalExprVar 0 False ""]
                            [gamma1]
                            [Ductive{..}]
                            [motive]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1)
                                   (1 + length gamma1)
                                   (substExprs 0 (map (shiftFreeVarsExpr (1 + length gamma1)
                                                                         (1 + length gamma1))
                                                      (reverse constrArgs))
                                                 (substExpr 0
                                                              (shiftFreeVarsExpr (1 + length gamma1)
                                                                                 (1 + length gamma1)
                                                                                  recEval)
                                                              (snd $ matches !! i)))
    (getExprArgs -> (Structor{num=i, ductive = duc2}, tau) , getExprArgs -> (c@Iter{..}, args))
      | inOrCoin ductive == IsCoin
        && inOrCoin duc2 == IsCoin
        && length tau == length (gamma1 $ strDefs ductive !! i) -> do
      let StrDef{..} = strDefs ductive !! i
          openDuctive = ductive
          parametersTyExpr = parameters
      recEval <- typeAction (substPars 0 (reverse parameters) a)
                            1
                            [applyExprArgs (c, map (shiftFreeVarsExpr 1 0)
                                               $ idCtx (gamma ductive))
                            :@: LocalExprVar 0 False ""]
                            [gamma1]
                            [motive]
                            [Ductive{..}]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1)
                                   (1 + length gamma1)
                                   (substExprs 0 (map (shiftFreeVarsExpr (1 + length gamma1)
                                                                         (1 + length gamma1))
                                                      (last args : reverse tau))
                                                 (substExpr 0 (snd $ matches !! i)
                                                              (shiftFreeVarsExpr (1 + length gamma1)
                                                                                 (1 + length gamma1)
                                                                                 recEval)))
    _ -> pure $ valF :@: valArg
evalExpr (GlobalExprVar v tyPars exprPars) = do
  valTyPars <- mapM evalTypeExpr tyPars
  valExprPars <- mapM evalExpr exprPars
  lookupDefExpr v valTyPars valExprPars >>= evalExpr
evalExpr e =
  overExprM evalFuns e

evalDuctive :: OpenDuctive -> Eval ann OpenDuctive
evalDuctive = overOpenDuctiveM evalFuns

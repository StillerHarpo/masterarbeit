{-# language RecordWildCards #-}

module Subst where

import Data.Bifunctor     (second)

import AbstractSyntaxTree
import ShiftFreeVars
import Mark

substExprFuns :: Int -> Expr -> OverFuns
substExprFuns i r =
  (overFuns (substExprFuns i r))
    { fTyExpr      = substTypeExpr i r
    , fOpenDuctive = substOpenDuctiveExpr i r
    , fStrDef      = substStrDefExpr i r
    , fExpr        = substExpr i r
    , fCtx         = substCtx i r }

substTypeExpr :: Int -> Expr -> TypeExpr -> TypeExpr
substTypeExpr i r1 (Abstr n t r2) =
  Abstr n (substTypeExpr i r1 t)
          (substTypeExpr (i+1) (shiftFreeVarsExpr 1 0 r1) r2)
substTypeExpr i r  e            =
  overTypeExpr (substExprFuns i r) e

substOpenDuctiveExpr :: Int -> Expr -> OpenDuctive -> OpenDuctive
substOpenDuctiveExpr i r op@OpenDuctive{..} =
  let r' = shiftFreeParsExpr (length parameterCtx) 0 r
  in op { gamma = map (substTypeExpr i r') gamma
        , strDefs = map (substStrDefExpr i r') strDefs
        }

substStrDefExpr :: Int -> Expr -> StrDef -> StrDef
substStrDefExpr i r1 strDef@StrDef{..} =
  strDef { sigma = map (substExpr (1+i+length gamma1)
                                  (shiftFreeVarsExpr (1+length gamma1)
                                                     0
                                                     r1))
                       sigma
         , a = substTypeExpr (i + length gamma1)
                             (shiftFreeVarsExpr (length gamma1)
                                                0
                                                r1)
                             a
        , gamma1 = substCtx i r1 gamma1}

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j False _)
  | i == j                = r
  | otherwise             = v
substExpr i r iter@Iter{..}  =
  let OpenDuctive{..} = ductive
  in iter { ductive = overOpenDuctive (substExprFuns i r) ductive
          , parameters = map (substTypeExpr i r) parameters
          , motive = substTypeExpr i r motive
          , matches  =
              zipWith (\j -> second (substExpr (i+j) (shiftFreeVarsExpr j 0 r)))
                      (map ((+1) . length . gamma1) strDefs)
                      matches}
substExpr i r e           =
  overExpr (substExprFuns i r) e

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) =
  substTypeExpr i r e : substCtx (i+1) (shiftFreeVarsExpr 1 0 r) ctx

substExprs :: Int -> [Expr] -> Expr -> Expr
substExprs i exprs =
  unmarkInExpr . substMult substExpr i (map markInExpr exprs)

substExprsInCtx :: Int -> [Expr] -> Ctx -> Ctx
substExprsInCtx i exprs =
  unmarkInCtx . substMult substCtx i (map markInExpr exprs)

substTypeExprs :: Int -> [Expr] -> TypeExpr -> TypeExpr
substTypeExprs  i exprs =
  unmarkInTyExpr . substMult substTypeExpr i (map markInExpr exprs)

substTypeFuns :: Int -> TypeExpr -> OverFuns
substTypeFuns i r =
  (overFuns (substTypeFuns i r))
    { fTyExpr = substType i r
    , fOpenDuctive = substOpenDuctiveTypeExpr i r
    , fStrDef  = substStrDefTypeExpr i r }

substType :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substType i r v@(LocalTypeVar j False _)
  | i == j      = r
  | otherwise   = v
substType i r e = overTypeExpr (substTypeFuns i r) e

substTypes :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substTypes i tyExprs =
  unmarkInTyExpr . substMult substType i (map markInTyExpr tyExprs)

substOpenDuctiveTypeExpr :: Int -> TypeExpr -> OpenDuctive -> OpenDuctive
substOpenDuctiveTypeExpr i r op@OpenDuctive{..} =
  let r' = shiftFreeParsTypeExpr (length parameterCtx) 0 r
  in op { gamma = map (substType i r') gamma
        , strDefs = map (substStrDefTypeExpr i r') strDefs }

substStrDefTypeExpr :: Int -> TypeExpr -> StrDef -> StrDef
substStrDefTypeExpr i r strDef@StrDef{..} =
  strDef { a = substType (i+1) (shiftFreeTypeVars 1 0 r) a }

substParFuns :: Int -> TypeExpr -> OverFuns
substParFuns i r = (overFuns (substParFuns i r))
                     { fTyExpr  = substPar i r
                     , fOpenDuctive = substOpenDuctivePar i r
                     , fStrDef  = substStrDefPar i r }

substPar :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substPar i r v@(Parameter j False _)
  | i == j     = r
  | otherwise  = v
substPar i r e = overTypeExpr (substParFuns i r) e

substPars :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substPars i tyExprs =
  unmarkInTyExpr . substMult substPar i (map markInTyExpr tyExprs)

substOpenDuctivePar :: Int -> TypeExpr -> OpenDuctive -> OpenDuctive
substOpenDuctivePar i r op@OpenDuctive{..} =
   let i' = i + length parameterCtx
       r' = shiftFreeParsTypeExpr (length parameterCtx) 0 r
   in op { parameterCtx = substParCtxPar i r parameterCtx
         , gamma = map (substPar i' r') gamma
         , strDefs = map (substStrDefPar i' r') strDefs }

substOpenDuctivePars :: Int -> [TypeExpr] -> OpenDuctive -> OpenDuctive
substOpenDuctivePars i tyExprs =
  unmarkInDuc . substMult substOpenDuctivePar i (map markInTyExpr tyExprs)

substParCtxPar :: Int -> TypeExpr -> TyCtx -> TyCtx
substParCtxPar _ _ []               =
  []
substParCtxPar  i r (ctx : parCtx) =
  overCtx (substParFuns i r) ctx : substParCtxPar (i+1) r parCtx

substStrDefPar :: Int -> TypeExpr -> StrDef -> StrDef
substStrDefPar i r strDef@StrDef{..} =
  strDef { a = substPar i (shiftFreeTypeVars 1 0 r) a
         , sigma = map (substParInExpr i (shiftFreeTypeVars 1 0 r)) sigma
         , gamma1 = substParInCtx i r gamma1 }

substStrDefPars :: Int -> [TypeExpr] -> StrDef -> StrDef
substStrDefPars i tyExprs =
  unmarkInStrDef . substMult substStrDefPar i (map markInTyExpr tyExprs)

substParInExpr :: Int -> TypeExpr -> Expr -> Expr
substParInExpr i r = overExpr (substParFuns i r)

substParsInExpr :: Int -> [TypeExpr] -> Expr -> Expr
substParsInExpr i tyExprs =
  unmarkInExpr . substMult substParInExpr i (map markInTyExpr tyExprs)

substParInCtx :: Int -> TypeExpr -> Ctx -> Ctx
substParInCtx i r = overCtx (substParFuns i r)

substParsInCtx :: Int -> [TypeExpr] -> Ctx -> Ctx
substParsInCtx = substMult substParInCtx

substParInParCtx :: Int -> TypeExpr -> TyCtx -> TyCtx
substParInParCtx _ _ []          =
  []
substParInParCtx i r (ctx':ctxs) =
  substParInCtx i r ctx' : substParInParCtx (i+1) r ctxs

-- TODO reverse list of expressions here instead of call side
-- | substitutes multiple variables in a expression
substMult :: (Int -> a -> b -> b) -> Int -> [a] -> b -> b
substMult _ _ []     e = e
substMult f n (v:vs) e = substMult f (n+1) vs (f n v e)

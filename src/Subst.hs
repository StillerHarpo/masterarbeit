{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Subst where

import AbstractSyntaxTree
import ShiftFreeVars

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j _)
  | i == j = r
  | otherwise = v
substExpr i r (GlobalExprVar n tyPars exprPars) =
  GlobalExprVar n (map (substTypeExpr i r) tyPars)
                  (map (substExpr i r) exprPars)
substExpr i r (e1 :@: e2) = substExpr i r e1 :@: substExpr i r e2
substExpr i r re@Rec{..} = let Ductive{..} = fromRec
                               newMatches  = zipWith (\i m -> substExpr i (shiftFreeVarsExpr i 0 r) m)
                                                     (map ((+1) . (+i) . length . gamma1) strDefs)
                                                     matches
                           in re { matches = newMatches}
substExpr i r c@Corec{..} = let Ductive{..} = toCorec
                                newMatches  = zipWith (\i m -> substExpr i (shiftFreeVarsExpr i 0 r) m)
                                                      (map ((+1) . (+i) . length . gamma1) strDefs)
                                                      matches
                            in c { matches = newMatches }
substExpr _ _ e = e

substTypeExpr :: Int -> Expr -> TypeExpr -> TypeExpr
substTypeExpr i r (GlobalTypeVar n pars) = GlobalTypeVar n $ map (substTypeExpr i r) pars
substTypeExpr i r1 (Abstr t r2) =
  Abstr (substTypeExpr i r1 t)
        (substTypeExpr (i+1) (shiftFreeVarsExpr 0 1 r1) r2)
substTypeExpr i r1 (In d) = In $ substDuctiveExpr i r1 d
substTypeExpr i r1 (Coin d) = Coin $ substDuctiveExpr i r1 d
substTypeExpr i r (e1 :@ e2) = substTypeExpr i r e1 :@ substExpr i r e2
substTypeExpr _ _ e = e

substDuctiveExpr :: Int -> Expr -> Ductive -> Ductive
substDuctiveExpr i r1 dIn@Ductive{..} =
  let dOut = Ductive
        { gamma = substCtx i r1 gamma
        , strDefs = map (substStrDefExpr i r1) strDefs
        , nameDuc = "???"
        }
  in if dOut == dIn then dIn else dOut

substStrDefExpr :: Int -> Expr -> StrDef -> StrDef
substStrDefExpr i r1 strDef@StrDef{..} =
  strDef { sigma = map (substExpr (1+i+length gamma1)
                                  (shiftFreeVarsExpr 0
                                                    (1+i+length gamma1)
                                                     r1))
                       sigma
         , a = substTypeExpr (i + length gamma1)
                             (shiftFreeVarsExpr 0
                                                (i + length gamma1)
                                                r1)
                             a
        , gamma1 = substCtx i r1 gamma1}

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) =
  substTypeExpr i r e : substCtx (i+1) (shiftFreeVarsExpr 1 0 r) ctx

substExprs :: Int -> [Expr] -> Expr -> Expr
substExprs _ [] e = e
substExprs n (v:vs) e = substExprs (n+1) vs (substExpr n v e)

substTypeExprs :: Int -> [Expr] -> TypeExpr -> TypeExpr
substTypeExprs _ [] e = e
substTypeExprs n (v:vs) e = substTypeExprs (n+1) vs (substTypeExpr n v e)

substType :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substType i r v@(LocalTypeVar j _)
  | i == j = r
  | otherwise = v
substType i r (GlobalTypeVar n vars) = GlobalTypeVar n $ map (substType i r) vars
substType i r1 (Abstr t r2) = Abstr (substType i r1 t) (substType i r1 r2)
substType i r (In d) = In $ substDuctiveTypeExpr i r d
substType i r (Coin d) = Coin $ substDuctiveTypeExpr i r d
substType i r (e1 :@ e2) = substType i r e1 :@ e2
substType _ _ e = e

substTypes :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substTypes _ [] e = e
substTypes n (v:vs) e = substTypes (n+1) vs (substType n v e)

substDuctiveTypeExpr :: Int -> TypeExpr -> Ductive -> Ductive
substDuctiveTypeExpr i r = overDuctive (substType i r)
                                       (substTypeInExpr i r)
                                       (substStrDefTypeExpr i r)

substStrDefTypeExpr :: Int -> TypeExpr -> StrDef -> StrDef
substStrDefTypeExpr i r strDef@StrDef{..} =
  strDef { a = substType (i+1) (shiftFreeTypeVars 1 0 r) a }

substTypeInExpr :: Int -> TypeExpr -> Expr -> Expr
substTypeInExpr i r (e1 :@: e2) = substTypeInExpr i r e1 :@: substTypeInExpr i r e2
substTypeInExpr i r c@Constructor{..} = c {ductive = substDuctiveTypeExpr i r ductive }
substTypeInExpr i r d@Destructor{..} = d {ductive = substDuctiveTypeExpr i r ductive }
substTypeInExpr i r re@Rec{..} = re { fromRec = substDuctiveTypeExpr i r fromRec
                                    , toRec = substType i r toRec
                                    }
substTypeInExpr i r c@Corec{..} = c { fromCorec = substType i r fromCorec
                                    , toCorec = substDuctiveTypeExpr i r toCorec
                                    }
substTypeInExpr i r (WithParameters ps e) = WithParameters (map (substType i r) ps)
                                                           (substTypeInExpr i r e)
substTypeInExpr _ _ atom = atom

substTypesInExpr :: Int -> [TypeExpr] -> Expr -> Expr
substTypesInExpr _ [] e = e
substTypesInExpr n (v:vs) e = substTypesInExpr (n+1) vs (substTypeInExpr n v e)


substPar :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substPar i r v@(Parameter j _)
  | i == j = r
  | otherwise = v
substPar i r (GlobalTypeVar n vars) = GlobalTypeVar n $ map (substPar i r) vars
substPar i r1 (Abstr t r2) = Abstr (substPar i r1 t) (substPar i r1 r2)
substPar i r (In d) = In $ substDuctivePar i r d
substPar i r (Coin d) = Coin $ substDuctivePar i r d
substPar i r (e1 :@ e2) = substPar i r e1 :@ substParInExpr i r e2
substPar _ _ e = e

substPars :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substPars _ [] e = e
substPars n (v:vs) e = substPars (n+1) vs (substPar n v e)

substDuctivePar :: Int -> TypeExpr -> Ductive -> Ductive
substDuctivePar i r = overDuctive (substPar i r)
                                  (substParInExpr i r)
                                  (substStrDefPar i r)

substStrDefPar :: Int -> TypeExpr -> StrDef -> StrDef
substStrDefPar i r strDef@StrDef{..} =
  strDef { a = substPar i (shiftFreeTypeVars 1 0 r) a
         , sigma = map (substParInExpr i (shiftFreeTypeVars 1 0 r)) sigma
         , gamma1 = substParInCtx i r gamma1 }

substParInExpr :: Int -> TypeExpr -> Expr -> Expr
substParInExpr i r (e1 :@: e2) = substParInExpr i r e1 :@: substParInExpr i r e2
substParInExpr i r c@Constructor{..} = c {ductive = substDuctivePar i r ductive }
substParInExpr i r d@Destructor{..} = d {ductive = substDuctivePar i r ductive }
substParInExpr i r re@Rec{..} = re { fromRec = substDuctivePar i r fromRec
                                   , toRec = substPar i r toRec
                                   }
substParInExpr i r c@Corec{..} = c { fromCorec = substPar i r fromCorec
                                   , toCorec = substDuctivePar i r toCorec
                                   }
substParInExpr i r (WithParameters ps e) = substParInExpr (i+length ps) r e
substParInExpr _ _ atom = atom

substParsInExpr :: Int -> [TypeExpr] -> Expr -> Expr
substParsInExpr _ [] e = e
substParsInExpr n (v:vs) e = substParsInExpr (n+1) vs (substParInExpr n v e)

substParInCtx :: Int -> TypeExpr -> Ctx -> Ctx
substParInCtx i r = map (substPar i r)

substParInParCtx :: Int -> TypeExpr -> TyCtx -> TyCtx
substParInParCtx i r []          = []
substParInParCtx i r (ctx':ctxs) =
  substParInCtx i r ctx' : substParInParCtx (i+1) r ctxs


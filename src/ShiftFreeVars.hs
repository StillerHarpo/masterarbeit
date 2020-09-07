{-# language RecordWildCards #-}

module ShiftFreeVars where

import AbstractSyntaxTree

shiftFreeVarsCtx :: Int -> Int -> Ctx -> Ctx
shiftFreeVarsCtx _ _ []       = []
shiftFreeVarsCtx j k (ty:tys) =
  shiftFreeVarsTypeExpr j k ty : shiftFreeVarsCtx j (k+1) tys

shiftFreeVarsTypeExpr :: Int -- ^ how much should they be shifted
                      -> Int -- ^ offset for free vars
                      -> TypeExpr -> TypeExpr
shiftFreeVarsTypeExpr j k (e1 :@ e2) = shiftFreeVarsTypeExpr j k e1 :@ shiftFreeVarsExpr j k e2
shiftFreeVarsTypeExpr j k (GlobalTypeVar n vars) =
  GlobalTypeVar n $ map (shiftFreeVarsTypeExpr j k) vars
shiftFreeVarsTypeExpr j k (Abstr ty body) =
  Abstr (shiftFreeVarsTypeExpr j k ty) (shiftFreeVarsTypeExpr j (k+1) body)
shiftFreeVarsTypeExpr _ _ e = e

shiftFreeTypeVars :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> TypeExpr -> TypeExpr
shiftFreeTypeVars j k v@(LocalTypeVar i n)
  | i < k = v
  | otherwise = LocalTypeVar (i+j) n
shiftFreeTypeVars j k (e1 :@ e2) = shiftFreeTypeVars j k e1 :@ e2
shiftFreeTypeVars j k (GlobalTypeVar n vars) =
  GlobalTypeVar n $ map (shiftFreeTypeVars j k) vars
shiftFreeTypeVars j k (Abstr ty body) =
  Abstr (shiftFreeTypeVars j k ty) (shiftFreeTypeVars j k body)
shiftFreeTypeVars j k (In d) = In $ shiftFreeTypeVarsDuc j k d
shiftFreeTypeVars j k (Coin d) = Coin $ shiftFreeTypeVarsDuc j k d
shiftFreeTypeVars _ _ e = e


shiftFreeTypeVarsDuc :: Int -- ^ how much should they be shifted
                     -> Int -- ^ offset for free vars
                     -> Ductive -> Ductive
shiftFreeTypeVarsDuc j k d = d { as = map (shiftFreeTypeVars j (k+1)) (as d) }

shiftFreeVarsExpr :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> Expr -> Expr
shiftFreeVarsExpr j k v@(LocalExprVar i n)
  | i < k = v
  | otherwise = LocalExprVar (i+j) n
-- TODO maybe GlobaleExprVar
shiftFreeVarsExpr j k (e1 :@: e2) = shiftFreeVarsExpr j k e1
                                    :@: shiftFreeVarsExpr j k e2
shiftFreeVarsExpr j k r@Rec{..} =
  r {matches = zipWith (shiftFreeVarsExpr j) (map ( (1+) . (k+) . length)
                                             (gamma1s fromRec))
                       matches }
shiftFreeVarsExpr j k c@Corec{..} =
  c {matches = zipWith (shiftFreeVarsExpr j) (map ( (1+) . (k+). length)
                                             (gamma1s toCorec))
                       matches }
shiftFreeVarsExpr _ _ e = e

shiftFreeParsKind :: Int -> Int -> Kind -> Kind
shiftFreeParsKind j k = map (shiftFreeParsTypeExpr j k)

shiftFreeParsTypeExpr :: Int -> Int -> TypeExpr -> TypeExpr
shiftFreeParsTypeExpr j k p@(Parameter i n)
  | i < k = p
  | otherwise = Parameter (i+j) n
shiftFreeParsTypeExpr j k expr = overTypeExpr (shiftFreeParsTypeExpr j k)
                                              (shiftFreeParsDuc j k)
                                              (shiftFreeParsExpr j k)
                                              expr

shiftFreeParsDuc :: Int -> Int -> Ductive -> Ductive
shiftFreeParsDuc j k = overDuctive (shiftFreeParsTypeExpr j k)
                                   (shiftFreeParsExpr j k)

shiftFreeParsExpr :: Int -> Int -> Expr -> Expr
shiftFreeParsExpr j k (WithParameters pars expr) =
  WithParameters (map (shiftFreeParsTypeExpr j k) pars)
                 (shiftFreeParsExpr j (k + length pars) expr)
shiftFreeParsExpr j k expr = overExpr (shiftFreeParsTypeExpr j k)
                                      (shiftFreeParsDuc j k)
                                      (shiftFreeParsExpr j k)
                                      expr


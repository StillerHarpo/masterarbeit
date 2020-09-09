{-# language RecordWildCards #-}

module ShiftFreeVars where

import AbstractSyntaxTree

shiftFreeVarsFuns :: Int -> Int -> OverFuns
shiftFreeVarsFuns j k =
  (overFuns (shiftFreeVarsFuns j k))
     { fTyExpr  = shiftFreeVarsTypeExpr j k
     , fExpr    = shiftFreeVarsExpr j k
     , fCtx     = shiftFreeVarsCtx j k }

shiftFreeVarsTypeExpr :: Int -- ^ how much should they be shifted
                      -> Int -- ^ offset for free vars
                      -> TypeExpr -> TypeExpr
shiftFreeVarsTypeExpr j k (Abstr ty body) =
  Abstr (shiftFreeVarsTypeExpr j k ty) (shiftFreeVarsTypeExpr j (k+1) body)
shiftFreeVarsTypeExpr j k e               =
  overTypeExpr (shiftFreeVarsFuns j k) e

shiftFreeVarsExpr :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> Expr -> Expr
shiftFreeVarsExpr j k v@(LocalExprVar i n)
  | i < k                         =
      v
  | otherwise                     =
      LocalExprVar (i+j) n
shiftFreeVarsExpr j k r@Rec{..}   =
    r { matches = zipWith (shiftFreeVarsExpr j) (map ((1+) . (k+)
                                                       . length . gamma1)
                                                (strDefs fromRec))
                         matches }
shiftFreeVarsExpr j k c@Corec{..} =
    c { matches = zipWith (shiftFreeVarsExpr j) (map ((1+) . (k+)
                                                      .  length . gamma1)
                                               (strDefs toCorec))
                         matches }
shiftFreeVarsExpr j k e            =
  overExpr (shiftFreeVarsFuns j k) e

shiftFreeVarsCtx :: Int -> Int -> Ctx -> Ctx
shiftFreeVarsCtx _ _ []       = []
shiftFreeVarsCtx j k (ty:tys) =
  shiftFreeVarsTypeExpr j k ty : shiftFreeVarsCtx j (k+1) tys

shiftFreeTypeVarsFuns :: Int -> Int -> OverFuns
shiftFreeTypeVarsFuns j k =
  (overFuns (shiftFreeTypeVarsFuns j k))
    { fTyExpr = shiftFreeTypeVars j k
    , fStrDef = shiftFreeTypeVarsStrDef j k }

shiftFreeTypeVars :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> TypeExpr -> TypeExpr
shiftFreeTypeVars j k v@(LocalTypeVar i n)
  | i < k               = v
  | otherwise           = LocalTypeVar (i+j) n
shiftFreeTypeVars j k e = overTypeExpr (shiftFreeTypeVarsFuns j k) e

shiftFreeTypeVarsStrDef :: Int -- ^ how much should they be shifted
                        -> Int -- ^ offset for free vars
                        -> StrDef -> StrDef
shiftFreeTypeVarsStrDef j k strDef = strDef { a = shiftFreeTypeVars j (k+1) (a strDef) }

shiftFreeParsKind :: Int -> Int -> Kind -> Kind
shiftFreeParsKind j k = map (shiftFreeParsTypeExpr j k)

shiftFreeParsFuns :: Int -> Int -> OverFuns
shiftFreeParsFuns j k =
  (overFuns (shiftFreeParsFuns j k))
    { fTyExpr = shiftFreeParsTypeExpr j k
    , fExpr = shiftFreeParsExpr j k }

shiftFreeParsTypeExpr :: Int -> Int -> TypeExpr -> TypeExpr
shiftFreeParsTypeExpr j k p@(Parameter i n)
  | i < k                      = p
  | otherwise                  = Parameter (i+j) n
shiftFreeParsTypeExpr j k e    = overTypeExpr (shiftFreeParsFuns j k) e

shiftFreeParsExpr :: Int -> Int -> Expr -> Expr
shiftFreeParsExpr j k (WithParameters pars expr) =
  WithParameters (map (shiftFreeParsTypeExpr j k) pars)
                 (shiftFreeParsExpr j (k + length pars) expr)
shiftFreeParsExpr j k expr                       =
  overExpr (shiftFreeParsFuns j k) expr

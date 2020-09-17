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
shiftFreeVarsExpr j k v@(LocalExprVar i m n)
  | i < k                         =
      v
  | otherwise                     =
      LocalExprVar (i+j) m n
shiftFreeVarsExpr j k i@Iter{..}   =
    i { matches = zipWith (shiftFreeVarsExpr j)
                          (map ((1+) . (k+) . length . gamma1)
                               (strDefs ductive))
                          matches
      , ductive = overOpenDuctive (shiftFreeVarsFuns j k) ductive
      , motive = overTypeExpr (shiftFreeVarsFuns j k) motive }
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
shiftFreeTypeVars j k v@(LocalTypeVar i False n)
  | i < k               = v
  | otherwise           = LocalTypeVar (i+j) False n
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
    , fOpenDuctive = shiftFreeParsOpenDuctive j k }

shiftFreeParsTypeExpr :: Int -> Int -> TypeExpr -> TypeExpr
shiftFreeParsTypeExpr j k p@(Parameter i m n)
  | i < k                      = p
  | otherwise                  = Parameter (i+j) m n
shiftFreeParsTypeExpr j k e    = overTypeExpr (shiftFreeParsFuns j k) e

shiftFreeParsOpenDuctive :: Int -> Int -> OpenDuctive -> OpenDuctive
shiftFreeParsOpenDuctive j k od@OpenDuctive{..} =
  let k' = k + length parameterCtx
  in  od { parameterCtx = shiftFreeParsParCtx j k parameterCtx
         , gamma = map (shiftFreeParsTypeExpr j k') gamma
         , strDefs = map (overStrDef (shiftFreeParsFuns j k')) strDefs }

shiftFreeParsExpr :: Int -> Int -> Expr -> Expr
shiftFreeParsExpr j k = overExpr (shiftFreeParsFuns j k)

shiftFreeParsParCtx :: Int -> Int -> TyCtx -> TyCtx
shiftFreeParsParCtx _ _ []               =
  []
shiftFreeParsParCtx j k (ctx : parCtx) =
  overCtx (shiftFreeParsFuns j k) ctx
  : shiftFreeParsParCtx j (k+1) parCtx

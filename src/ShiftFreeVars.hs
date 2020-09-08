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
shiftFreeVarsTypeExpr j k (Abstr ty body) =
  Abstr (shiftFreeVarsTypeExpr j k ty) (shiftFreeVarsTypeExpr j (k+1) body)
shiftFreeVarsTypeExpr j k e = overTypeExpr (shiftFreeVarsTypeExpr j k)
                                           id -- TODO Don't we need ductive here
                                           (shiftFreeVarsExpr j k)
                                           e

shiftFreeVarsExpr :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> Expr -> Expr
shiftFreeVarsExpr j k v@(LocalExprVar i n)
  | i < k = v
  | otherwise = LocalExprVar (i+j) n
shiftFreeVarsExpr j k r@Rec{..} =
  r { matches = zipWith (shiftFreeVarsExpr j) (map ((1+) . (k+)
                                                     . length . gamma1)
                                              (strDefs fromRec))
                       matches }
shiftFreeVarsExpr j k c@Corec{..} =
  c { matches = zipWith (shiftFreeVarsExpr j) (map ((1+) . (k+)
                                                    .  length . gamma1)
                                             (strDefs toCorec))
                       matches }
shiftFreeVarsExpr j k e = overExpr (shiftFreeVarsTypeExpr j k)
                                   id -- TODO Don't we need ductive here
                                   (shiftFreeVarsExpr j k)
                                   e

shiftFreeTypeVars :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> TypeExpr -> TypeExpr
shiftFreeTypeVars j k v@(LocalTypeVar i n)
  | i < k = v
  | otherwise = LocalTypeVar (i+j) n
shiftFreeTypeVars j k e = overTypeExpr (shiftFreeTypeVars j k)
                                       (shiftFreeTypeVarsDuc j k)
                                       id -- TODO Don't we need expr here
                                       e

shiftFreeTypeVarsDuc :: Int -- ^ how much should they be shifted
                     -> Int -- ^ offset for free vars
                     -> Ductive -> Ductive
shiftFreeTypeVarsDuc j k = overDuctive (shiftFreeTypeVars j k)
                                       id -- TODO Don't we need expr here
                                       (shiftFreeTypeVarsStrDef j k)

shiftFreeTypeVarsStrDef :: Int -- ^ how much should they be shifted
                        -> Int -- ^ offset for free vars
                        -> StrDef -> StrDef
shiftFreeTypeVarsStrDef j k strDef = strDef { a = shiftFreeTypeVars j (k+1) (a strDef) }

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
                                   (shiftFreeParsStrDef j k)

shiftFreeParsStrDef :: Int -> Int -> StrDef -> StrDef
shiftFreeParsStrDef j k = overStrDef (shiftFreeParsTypeExpr j k)
                                     (shiftFreeParsExpr j k)


shiftFreeParsExpr :: Int -> Int -> Expr -> Expr
shiftFreeParsExpr j k (WithParameters pars expr) =
  WithParameters (map (shiftFreeParsTypeExpr j k) pars)
                 (shiftFreeParsExpr j (k + length pars) expr)
shiftFreeParsExpr j k expr = overExpr (shiftFreeParsTypeExpr j k)
                                      (shiftFreeParsDuc j k)
                                      (shiftFreeParsExpr j k)
                                      expr


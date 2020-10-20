{-# language TypeOperators #-}
{-# language RecordWildCards #-}

module AbstractSyntaxTree where

import           Data.Text              (Text)

import           Control.Monad.Identity

import           Lib

type Type = (Ctx,TypeExpr) -- Ctx = Gamma, TypeExpr = A in "Gamma -> A"
type Kind = Ctx -- Ctx = Gamma in "Gamma -> *"

data TypedExpr = TypedExpr Expr Type
  deriving (Show)

-- (Con/De)structor-Context
type StrCtx = [(Text,Type)]

data Decl = ExprDef { name :: Text
                    , tyParameterCtx :: TyCtx
                    , exprParameterCtx :: Ctx
                    , expr :: Expr
                    , ty :: Maybe Type
                    }
          | TypeDef OpenDuctive
          | Expression Expr
  deriving (Eq,Show)

data TypeExpr = UnitType -- verum type
              | TypeExpr :@ Expr
              -- If numbers are the same the names should also be the
              -- same
              -- TODO Maybe throw error if this is not the case
              | LocalTypeVar Int Bool Text
              | Parameter Int Bool Text
              | GlobalTypeVar Text [TypeExpr]
              | Abstr Text TypeExpr TypeExpr
              | Ductive { openDuctive :: OpenDuctive
                        , parametersTyExpr :: [TypeExpr]}
  deriving (Show)

instance Eq TypeExpr where
  UnitType              == UnitType              = True
  (tE1 :@ e1)           == (tE2 :@ e2)           = tE1 == tE2 && e1 == e2
  (LocalTypeVar i1 _ _) == (LocalTypeVar i2 _ _) = i1 == i2
  (Parameter i1 _ _)    == (Parameter i2 _ _)    = i1 == i2
  (GlobalTypeVar n1 p1) == (GlobalTypeVar n2 p2) = n1 == n2 && p1 == p2
  (Abstr _ ty1 e1)      == (Abstr _ ty2 e2)      = ty1 == ty2 && e1 == e2
  (Ductive oDuc1 pars1) == (Ductive oDuc2 pars2) = oDuc1 == oDuc2
                                                   && pars1 == pars2
  _                     == _                     = False

data OpenDuctive = OpenDuctive { nameDuc :: Text
                               , inOrCoin :: InOrCoin
                               , parameterCtx :: TyCtx
                               , gamma :: Ctx
                               , strDefs :: [StrDef]
                               }
  deriving (Show,Eq)

data StrDef = StrDef { sigma :: [Expr]
                     , a :: TypeExpr
                     , gamma1 :: Ctx
                     , strName :: Text
                     }
  deriving (Show,Eq)

data InOrCoin = IsIn | IsCoin
  deriving (Show,Eq)

data Expr = UnitExpr -- verum value
          -- If numbers are the same the names should also be the
          -- same
          -- TODO Maybe throw error if this is not the case
          | LocalExprVar Int Bool Text -- Ctx-- term variables
          | GlobalExprVar Text [TypeExpr] [Expr]
          | Expr :@: Expr
          | Structor { ductive :: OpenDuctive
                     , parameters :: [TypeExpr]
                     , num :: Int
                     }
          | Iter { ductive :: OpenDuctive
                 , parameters :: [TypeExpr]
                 , motive :: TypeExpr
                 , matches :: [([Text],Expr)]
                 }
  deriving (Show)

instance Eq Expr where
  UnitExpr                 == UnitExpr                 =
    True
  (LocalExprVar i _ _)     == (LocalExprVar j _ _)     =
    i == j
  (GlobalExprVar n p1 pe1) == (GlobalExprVar m p2 pe2) =
    n == m && p1 == p2 && pe1 ==pe2
  (e1 :@: e2)              == (e3 :@: e4)              =
    e1 == e3 && e2 == e4
  (Structor d1 ps1 i1)     == (Structor d2 ps2 i2)     =
    d1 == d2 && ps1 == ps2 && i1 == i2
  (Iter fr1 ps1 tr1 ms1)   == (Iter fr2 ps2 tr2 ms2)   =
    fr1 == fr2 && ps1 == ps2 && tr1 == tr2 && ms1 == ms2
  _                        == _                        =
    False

type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [Ctx]

data OverFuns = OverFuns { fTyExpr      :: TypeExpr    -> TypeExpr
                         , fOpenDuctive :: OpenDuctive -> OpenDuctive
                         , fStrDef      :: StrDef      -> StrDef
                         , fExpr        :: Expr        -> Expr
                         , fParCtx      :: TyCtx       -> TyCtx
                         , fCtx         :: Ctx         -> Ctx }

data OverFunsM m = OverFunsM
  { fTyExprM      :: TypeExpr     -> m TypeExpr
  , fOpenDuctiveM :: OpenDuctive  -> m OpenDuctive
  , fStrDefM      :: StrDef       -> m StrDef
  , fExprM        :: Expr         -> m Expr
  , fParCtxM      :: TyCtx        -> m TyCtx
  , fCtxM         :: Ctx          -> m Ctx }

-- TODO Doesn't work that way
overFuns :: OverFuns -> OverFuns
overFuns f = OverFuns { fTyExpr      = overTypeExpr f
                      , fOpenDuctive = overOpenDuctive f
                      , fStrDef      = overStrDef f
                      , fExpr        = overExpr f
                      , fParCtx      = overParCtx f
                      , fCtx         = overCtx f }

overFunsM :: Monad m => OverFunsM m -> OverFunsM m
overFunsM f = OverFunsM { fTyExprM      = overTypeExprM f
                        , fOpenDuctiveM = overOpenDuctiveM f
                        , fStrDefM      = overStrDefM f
                        , fExprM        = overExprM f
                        , fParCtxM      = overParCtxM f
                        , fCtxM         = overCtxM f }

overFunsToOverFunsM :: OverFuns -> OverFunsM Identity
overFunsToOverFunsM OverFuns{..} =
  OverFunsM { fTyExprM      = pure . fTyExpr
            , fOpenDuctiveM = pure . fOpenDuctive
            , fStrDefM      = pure . fStrDef
            , fExprM        = pure . fExpr
            , fParCtxM      = pure . fParCtx
            , fCtxM         = pure . fCtx }

overTypeExpr :: OverFuns -> TypeExpr -> TypeExpr
overTypeExpr overFuns =
  runIdentity . overTypeExprM (overFunsToOverFunsM overFuns)

overTypeExprM :: Monad m => OverFunsM m -> TypeExpr -> m TypeExpr
overTypeExprM OverFunsM{..} (tyExpr :@ expr)          =
  (:@) <$> fTyExprM tyExpr <*> fExprM expr
overTypeExprM OverFunsM{..} (GlobalTypeVar n tyExprs) =
  GlobalTypeVar n <$> mapM fTyExprM tyExprs
overTypeExprM OverFunsM{..} (Abstr n tyExpr1 tyExpr2) =
  Abstr n <$> fTyExprM tyExpr1 <*> fTyExprM tyExpr2
overTypeExprM OverFunsM{..} Ductive{..}               =
  do openDuctive <- fOpenDuctiveM openDuctive
     parametersTyExpr <- mapM fTyExprM parametersTyExpr
     pure Ductive{..}
overTypeExprM _             atom =
  pure atom

overOpenDuctive :: OverFuns -> OpenDuctive -> OpenDuctive
overOpenDuctive overFuns =
  runIdentity . overOpenDuctiveM (overFunsToOverFunsM overFuns)

overOpenDuctiveM :: Monad m => OverFunsM m -> OpenDuctive -> m OpenDuctive
overOpenDuctiveM OverFunsM{..} OpenDuctive{..} = do
  parameterCtx <- fParCtxM parameterCtx
  gamma <- fCtxM gamma
  strDefs <- mapM fStrDefM strDefs
  pure OpenDuctive{..}

overStrDef :: OverFuns -> StrDef -> StrDef
overStrDef overFuns =
  runIdentity . overStrDefM (overFunsToOverFunsM overFuns)

overStrDefM :: Monad m => OverFunsM m -> StrDef -> m StrDef
overStrDefM OverFunsM{..} StrDef{..} = do
  sigma <- mapM fExprM sigma
  a <- fTyExprM a
  gamma1 <- fCtxM gamma1
  pure StrDef{..}

overParCtx :: OverFuns -> TyCtx -> TyCtx
overParCtx OverFuns{..} = map fCtx

overParCtxM :: Monad m => OverFunsM m -> TyCtx -> m TyCtx
overParCtxM OverFunsM{..} = mapM fCtxM

overCtx :: OverFuns -> Ctx -> Ctx
overCtx OverFuns{..} = map fTyExpr

overCtxM :: Monad m => OverFunsM m -> Ctx -> m Ctx
overCtxM OverFunsM{..} = mapM fTyExprM

overExpr :: OverFuns -> Expr -> Expr
overExpr overFuns =
  runIdentity . overExprM (overFunsToOverFunsM overFuns)

overExprM :: Monad m => OverFunsM m -> Expr -> m Expr
overExprM OverFunsM{..} (GlobalExprVar n tyExprs exprs) =
  GlobalExprVar n <$> mapM fTyExprM tyExprs <*> mapM fExprM exprs
overExprM OverFunsM{..} (expr1 :@: expr2)               =
  (:@:) <$> fExprM expr1 <*> fExprM expr2
overExprM OverFunsM{..} Structor{..}                    =
  do ductive <- fOpenDuctiveM ductive
     parameters <- mapM fTyExprM parameters
     pure Structor{..}
overExprM OverFunsM{..} Iter{..}                        =
  do ductive <- fOpenDuctiveM ductive
     parameters <- mapM fTyExprM parameters
     motive <- fTyExprM motive
     matches <- mapM (secondM fExprM) matches
     pure Iter{..}
overExprM OverFunsM{..} atom                            =
  pure atom

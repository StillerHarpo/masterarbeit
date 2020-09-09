{-# language TypeOperators #-}
{-# language RecordWildCards #-}

module AbstractSyntaxTree where

import           Data.Text                   (Text)
import qualified Data.Text              as T
import           Data.Map                    (Map)

import           Control.Monad.Identity

type Type = (Ctx,TypeExpr) -- Ctx = Gamma, TypeExpr = A in "Gamma -> A"
type Kind = Ctx -- Ctx = Gamma in "Gamma -> *"

data TypedExpr = TypedExpr Expr Type
  deriving (Show)

type StrCtx = [(Text,Type)]

data Statement = ExprDef { name :: Text
                         , tyParameterCtx :: TyCtx
                         , exprParameterCtx :: Ctx
                         , expr :: Expr
                         , ty :: Maybe Type
                         }
               | TypeDef { name :: Text
                         , parameterCtx :: TyCtx
                         , typeExpr :: TypeExpr
                         , kind :: Maybe Kind
                         }
               | Expression Expr
  deriving (Eq,Show)

data TypeExpr = UnitType -- verum type
              | TypeExpr :@ Expr
              -- If numbers are the same the names should also be the
              -- same
              -- TODO Maybe throw error if this is not the case
              | LocalTypeVar Int Text
              | Parameter Int Text
              | GlobalTypeVar Text [TypeExpr]
              | Abstr TypeExpr TypeExpr
              | In Ductive
              | Coin Ductive
  deriving (Show)

instance Eq TypeExpr where
  UnitType              == UnitType              = True
  (tE1 :@ e1)           == (tE2 :@ e2)           = tE1 == tE2 && e1 == e2
  (LocalTypeVar i1 _)   == (LocalTypeVar i2 _)   = i1 == i2
  (Parameter i1 _)      == (Parameter i2 _)      = i1 == i2
  (GlobalTypeVar n1 p1) == (GlobalTypeVar n2 p2) = n1 == n2 && p1 == p2
  (Abstr ty1 e1)        == (Abstr ty2 e2)        = ty1 == ty2 && e1 == e2
  (In d1)               == (In d2)               = d1 == d2
  (Coin d1)             == (Coin d2)             = d1 == d2
  _                     == _                     = False

data Ductive = Ductive { gamma :: Ctx
                       , strDefs :: [StrDef]
                       , nameDuc :: Text
                       }
  deriving (Show)

instance Eq Ductive where
  Ductive g1 s1 _ == Ductive g2 s2 _ = g1 == g2 && s1 == s2

data StrDef = StrDef { sigma :: [Expr]
                     , a :: TypeExpr
                     , gamma1 :: Ctx
                     , strName :: Text
                     }
  deriving (Show)

instance Eq StrDef where
  StrDef s1 a1 g1 _ == StrDef s2 a2 g2 _ =
    s1 == s2 && a1 == a2 && g1 == g2

data Expr = UnitExpr -- verum value
          -- If numbers are the same the names should also be the
          -- same
          -- TODO Maybe throw error if this is not the case
          | LocalExprVar Int Text -- Ctx-- term variables
          | GlobalExprVar Text [TypeExpr] [Expr]
          | Expr :@: Expr
          | Constructor { ductive :: Ductive
                        , num :: Int
                        }
          | Destructor { ductive :: Ductive
                       , num :: Int
                       }
          | Rec { fromRec :: Ductive
                , toRec :: TypeExpr
                , matches :: [Expr]
                }
          | Corec { fromCorec :: TypeExpr
                  , toCorec :: Ductive
                  , matches :: [Expr]
                  }
          | WithParameters [TypeExpr] Expr
  deriving (Show)

instance Eq Expr where
  UnitExpr                 == UnitExpr                 =
    True
  (LocalExprVar i _)       == (LocalExprVar j _)       =
    i == j
  (GlobalExprVar n p1 pe1) == (GlobalExprVar m p2 pe2) =
    n == m && p1 == p2 && pe1 ==pe2
  (e1 :@: e2)              == (e3 :@: e4)              =
    e1 == e3 && e2 == e4
  (Constructor d1 i1)      == (Constructor d2 i2)      =
    d1 == d2 && i1 == i2
  (Destructor d1 i1)       == (Destructor d2 i2)       =
    d1 == d2 && i1 == i2
  (Rec fr1 tr1 ms1)        == (Rec fr2 tr2 ms2)        =
    fr1 == fr2 && tr1 == tr2 && and (zipWith (==) ms1 ms2)
  (Corec fc1 tc1 ms1)      == (Corec fc2 tc2 ms2)      =
    fc1 == fc2 && tc1 == tc2 && and (zipWith (==) ms1 ms2)
  (WithParameters te1 e1)  == (WithParameters te2 e2)  =
    te1 == te2 && e1 == e2
  _                        == _                        =
    False


type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [Ctx]

data OverFuns = OverFuns { fTyExpr  :: TypeExpr -> TypeExpr
                         , fDuctive :: Ductive  -> Ductive
                         , fStrDef  :: StrDef   -> StrDef
                         , fExpr    :: Expr     -> Expr
                         , fCtx     :: Ctx      -> Ctx }

data OverFunsM m = OverFunsM { fTyExprM  :: TypeExpr -> m TypeExpr
                             , fDuctiveM :: Ductive  -> m Ductive
                             , fStrDefM  :: StrDef   -> m StrDef
                             , fExprM    :: Expr     -> m Expr
                             , fCtxM     :: Ctx      -> m Ctx }

-- TODO Doesn't work that way
overFuns :: OverFuns -> OverFuns
overFuns f = OverFuns { fTyExpr  = overTypeExpr f
                      , fDuctive = overDuctive f
                      , fStrDef  = overStrDef f
                      , fExpr    = overExpr f
                      , fCtx     = overCtx f }

overFunsM :: Monad m => OverFunsM m -> OverFunsM m
overFunsM f = OverFunsM { fTyExprM  = overTypeExprM f
                        , fDuctiveM = overDuctiveM f
                        , fStrDefM  = overStrDefM f
                        , fExprM    = overExprM f
                        , fCtxM     = overCtxM f }

overFunsToOverFunsM :: OverFuns -> OverFunsM Identity
overFunsToOverFunsM OverFuns{..} =
  OverFunsM { fTyExprM  = pure . fTyExpr
            , fDuctiveM = pure . fDuctive
            , fStrDefM  = pure . fStrDef
            , fExprM    = pure . fExpr
            , fCtxM     = pure . fCtx }

overTypeExpr :: OverFuns -> TypeExpr -> TypeExpr
overTypeExpr overFuns =
  runIdentity . overTypeExprM (overFunsToOverFunsM overFuns)

overTypeExprM :: Monad m => OverFunsM m -> TypeExpr -> m TypeExpr
overTypeExprM OverFunsM{..} (tyExpr :@ expr)          =
  (:@) <$> fTyExprM tyExpr <*> fExprM expr
overTypeExprM OverFunsM{..} (GlobalTypeVar n tyExprs) =
  GlobalTypeVar n <$> mapM fTyExprM tyExprs
overTypeExprM OverFunsM{..} (Abstr tyExpr1 tyExpr2)   =
  Abstr <$> fTyExprM tyExpr1 <*> fTyExprM tyExpr2
overTypeExprM OverFunsM{..} (In duc)                  =
  In <$> fDuctiveM duc
overTypeExprM OverFunsM{..} (Coin duc)                =
  Coin <$> fDuctiveM duc
overTypeExprM _             atom =
  pure atom

overDuctive :: OverFuns -> Ductive -> Ductive
overDuctive overFuns =
  runIdentity . overDuctiveM (overFunsToOverFunsM overFuns)

overDuctiveM :: Monad m => OverFunsM m -> Ductive -> m Ductive
overDuctiveM OverFunsM{..} Ductive{..} = do
  gamma <- fCtxM gamma
  strDefs <- mapM fStrDefM strDefs
  pure Ductive{..}

overStrDef :: OverFuns -> StrDef -> StrDef
overStrDef overFuns =
  runIdentity . overStrDefM (overFunsToOverFunsM overFuns)

overStrDefM :: Monad m => OverFunsM m -> StrDef -> m StrDef
overStrDefM OverFunsM{..} StrDef{..} = do
  sigma <- mapM fExprM sigma
  a <- fTyExprM a
  gamma1 <- fCtxM gamma1
  pure StrDef{..}

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
overExprM OverFunsM{..} Constructor{..}                 =
  do ductive <- fDuctiveM ductive
     pure Constructor{..}
overExprM OverFunsM{..} Destructor{..}                  =
  do ductive <- fDuctiveM ductive
     pure Destructor{..}
overExprM OverFunsM{..} Rec{..}                         =
  do fromRec <- fDuctiveM fromRec
     toRec <- fTyExprM toRec
     matches <- mapM fExprM matches
     pure Rec{..}
overExprM OverFunsM{..} Corec{..}                       =
  do fromCorec <- fTyExprM fromCorec
     toCorec <- fDuctiveM toCorec
     matches <- mapM fExprM matches
     pure Corec{..}
overExprM OverFunsM{..} (WithParameters pars expr)      =
  WithParameters <$> mapM fTyExprM pars <*> fExprM expr
overExprM OverFunsM{..} atom                            =
  pure atom

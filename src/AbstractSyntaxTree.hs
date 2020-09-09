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

overTypeExpr :: (TypeExpr -> TypeExpr)
             -> (Ductive -> Ductive)
             -> (Expr -> Expr)
             -> TypeExpr -> TypeExpr
overTypeExpr fTyExpr fDuc fExpr =
  runIdentity . overTypeExprM (pure . fTyExpr)
                              (pure . fDuc)
                              (pure . fExpr)

overTypeExprM :: Monad m
              => (TypeExpr -> m TypeExpr)
              -> (Ductive -> m Ductive)
              -> (Expr -> m Expr)
              -> TypeExpr -> m TypeExpr
overTypeExprM fTyExpr _   fExpr (tyExpr :@ expr)          =
  (:@) <$> fTyExpr tyExpr <*> fExpr expr
overTypeExprM fTyExpr _   _     (GlobalTypeVar n tyExprs) =
  GlobalTypeVar n <$> mapM fTyExpr tyExprs
overTypeExprM fTyExpr _   _     (Abstr tyExpr1 tyExpr2)   =
  Abstr <$> fTyExpr tyExpr1 <*> fTyExpr tyExpr2
overTypeExprM _      fDuc _     (In duc)                  =
  In <$> fDuc duc
overTypeExprM _      fDuc _     (Coin duc)                =
  Coin <$> fDuc duc
overTypeExprM _      _    _     atom                      =
  pure atom

overDuctive :: (TypeExpr -> TypeExpr)
             -> (Expr -> Expr)
             -> (StrDef -> StrDef)
             -> Ductive -> Ductive
overDuctive fTyExpr fExpr fStrDef =
  runIdentity . overDuctiveM (pure . fTyExpr)
                             (pure . fExpr)
                             (pure . fStrDef)

overDuctiveM :: Monad m
             => (TypeExpr -> m TypeExpr)
             -> (Expr -> m Expr)
             -> (StrDef -> m StrDef)
             -> Ductive -> m Ductive
overDuctiveM fTyExpr fExpr fStrDef Ductive{..} = do
  gamma <- overTypeExprInCtxM fTyExpr gamma
  strDefs <- mapM fStrDef strDefs
  pure Ductive{..}

overStrDef :: (TypeExpr -> TypeExpr)
             -> (Expr -> Expr)
             -> StrDef -> StrDef
overStrDef fTyExpr fExpr =
  runIdentity . overStrDefM (pure . fTyExpr) (pure . fExpr)

overStrDefM :: Monad m
             => (TypeExpr -> m TypeExpr)
             -> (Expr -> m Expr)
             -> StrDef -> m StrDef
overStrDefM fTyExpr fExpr StrDef{..} = do
  sigma <- mapM fExpr sigma
  a <- fTyExpr a
  gamma1 <- overTypeExprInCtxM fTyExpr gamma1
  pure StrDef{..}

overTypeExprInCtxM :: Monad m => (TypeExpr -> m TypeExpr) -> Ctx -> m Ctx
overTypeExprInCtxM = mapM

overExpr :: (TypeExpr -> TypeExpr)
         -> (Ductive -> Ductive)
         -> (Expr -> Expr)
         -> Expr -> Expr
overExpr fTyExpr fDuc fExpr =
  runIdentity . overExprM (pure . fTyExpr)
                          (pure . fDuc)
                          (pure . fExpr)

overExprM :: Monad m
          => (TypeExpr -> m TypeExpr)
          -> (Ductive -> m Ductive)
          -> (Expr -> m Expr)
          -> Expr -> m Expr
overExprM fTyExpr _    fExpr (GlobalExprVar n tyExprs exprs) =
  GlobalExprVar n <$> mapM fTyExpr tyExprs <*> mapM fExpr exprs
overExprM _       _    fExpr (expr1 :@: expr2)               =
  (:@:) <$> fExpr expr1 <*> fExpr expr2
overExprM _       fDuc _     Constructor{..}                 =
  do ductive <- fDuc ductive
     pure Constructor{..}
overExprM _       fDuc _     Destructor{..}                  =
  do ductive <- fDuc ductive
     pure Destructor{..}
overExprM fTyExpr fDuc fExpr Rec{..}                         =
  do fromRec <- fDuc fromRec
     toRec <- fTyExpr toRec
     matches <- mapM fExpr matches
     pure Rec{..}
overExprM fTyExpr fDuc fExpr Corec{..}                       =
  do fromCorec <- fTyExpr fromCorec
     toCorec <- fDuc toCorec
     matches <- mapM fExpr matches
     pure Corec{..}
overExprM fTyExpr _    fExpr (WithParameters pars expr)      =
  WithParameters <$> mapM fTyExpr pars <*> fExpr expr
overExprM _       _    _     atom                            =
  pure atom

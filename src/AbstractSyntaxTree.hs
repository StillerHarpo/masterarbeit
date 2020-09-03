{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)

type Type = (Ctx,TypeExpr)
type Kind = Ctx

data TypedExpr = TypedExpr Expr Type
  deriving (Show)

type StrCtx = [(Text,Type)]

data Statement = ExprDef { name :: Text
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
                       , sigmas :: [[Expr]]
                       , as :: [TypeExpr]
                       , gamma1s :: [Ctx]
                       , nameDuc :: Text
                       , strNames :: [Text]
                       }
  deriving (Show)

instance Eq Ductive where
  Ductive g1 s1 a1 g11 _ _ == Ductive g2 s2 a2 g12 _ _ =
    g1 == g2 && s1 == s2 && a1 == a2 && g11 == g12

data Expr = UnitExpr -- verum value
          -- If numbers are the same the names should also be the
          -- same
          -- TODO Maybe throw error if this is not the case
          | LocalExprVar Int Text -- Ctx-- term variables
          | GlobalExprVar Text
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
  UnitExpr                == UnitExpr                = True
  (LocalExprVar i _)      == (LocalExprVar j _)      = i == j
  (GlobalExprVar n)       == (GlobalExprVar m)       = n == m
  (e1 :@: e2)             == (e3 :@: e4)             = e1 == e3 && e2 == e4
  (Constructor d1 i1)     == (Constructor d2 i2)     = d1 == d2 && i1 == i2
  (Destructor d1 i1)      == (Destructor d2 i2)      = d1 == d2 && i1 == i2
  (Rec fr1 tr1 ms1)       == (Rec fr2 tr2 ms2)       = fr1 == fr2 && tr1 == tr2
                                                       && and (zipWith (==) ms1 ms2)
  (Corec fc1 tc1 ms1)     == (Corec fc2 tc2 ms2)     = fc1 == fc2 && tc1 == tc2
                                                       && and (zipWith (==) ms1 ms2)
  (WithParameters te1 e1) == (WithParameters te2 e2) = te1 == te2 && e1 == e2
  _                       == _                       = False


type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [Ctx]

{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import Data.Map (Map)

type Type = (Ctx,TypeExpr)
type Kind = Ctx

type StrCtx = [(Text,Type)]

data Statement = ExprDef { name :: Text
                         , expr :: Expr
                         , ty :: Maybe Type
                         }
               | InductiveDef { name :: Text
                              , gamma :: Ctx
                              , sigmas :: [[Expr]]
                              , as :: [TypeExpr]
                              , constructors :: [Text]
                              , gamma1s :: [Ctx]
                              }
               | CoinductiveDef { name :: Text
                                , gamma :: Ctx
                                , sigmas :: [[Expr]]
                                , as :: [TypeExpr]
                                , destructors :: [Text]
                                , gamma1s :: [Ctx]
                                }
               | Expression Expr
  deriving (Eq, Show)

data TypeExpr = UnitType -- verum type
              | TypeExpr :@ Expr
              | TypeVar Text
              | Abstr TypeExpr TypeExpr
              | Inductive Text
              | Coinductive Text
  deriving (Eq, Show)

data Expr = UnitExpr -- verum value
          | LocalExprVar Int -- Ctx-- term variables
          | GlobalExprVar Text
          | Expr :@: Expr
          | Constructor Text
          | Destructor Text
          | Rec { fromRec :: Text
                , toRec :: TypeExpr
                , matches :: [Match]
                }
          | Corec { fromCorec :: TypeExpr
                  , toCorec :: Text
                  , matches :: [Match]
                  }
  deriving (Eq, Show)

data Match = Match {
    structorName  :: Text
  , matchExpr     :: Expr
  }
  deriving (Eq, Show)

type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [(Text, Ctx)]

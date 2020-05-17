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
               | TypeDef { name :: Text
                         , typeExpr :: TypeExpr
                         , kind :: Maybe Kind
                         }
               | Expression Expr
  deriving (Eq, Show)

data TypeExpr = UnitType -- verum type
              | TypeExpr :@ Expr
              | LocalTypeVar Int
              | GlobalTypeVar Text
              | Abstr TypeExpr TypeExpr
              | In Ductive
              | Coin Ductive
  deriving (Eq, Show)

data Ductive = Ductive { gamma :: Ctx
                       , sigmas :: [[Expr]]
                       , as :: [TypeExpr]
                       , gamma1s :: [Ctx]
                       }
  deriving (Eq, Show)

data Expr = UnitExpr -- verum value
          | LocalExprVar Int -- Ctx-- term variables
          | GlobalExprVar Text
          | Expr :@: Expr
          | Constructor Ductive Int
          | Destructor Ductive Int
          | Rec { fromRec :: Ductive
                , toRec :: TypeExpr
                , matches :: [Expr]
                }
          | Corec { fromCorec :: TypeExpr
                  , toCorec :: Ductive
                  , matches :: [Expr]
                  }
  deriving (Eq, Show)

type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [Ctx]

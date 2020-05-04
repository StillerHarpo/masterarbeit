{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import Data.Map (Map)

type Type = (Ctx,Expr)
type Kind = Ctx

type StrCtx = [(Text,Type)]

data Statement = ExprDef { name :: Text
                         , expr :: Expr
                         , ty :: Maybe Type
                         }
               | InductiveDef { name :: Text
                              , gamma :: Ctx
                              , sigmas :: [[Expr]]
                              , as :: [Expr]
                              , constructors :: [Text]
                              , gamma1s :: [Ctx]
                              }
               | CoinductiveDef { name :: Text
                                , gamma :: Ctx
                                , sigmas :: [[Expr]]
                                , as :: [Expr]
                                , destructors :: [Text]
                                , gamma1s :: [Ctx]
                                }
               | Expression Expr
  deriving (Eq, Show)

data Expr = UnitType -- verum value
          | UnitExpr -- verum type
          | LocalExprVar Int -- Ctx-- term variables
          | GlobalExprVar Text
          | Expr :@: Expr
          | Abstr Expr Expr
          | TypeVar Text
          | Constructor Text
          | Destructor Text
          | Inductive Text
          | Coinductive Text
          | Rec { from :: Expr
                , to :: Expr
                , matches :: [Match]
                }
          | Corec { from :: Expr
                  , to :: Expr
                  , matches :: [Match]
                  }
  deriving (Eq, Show)

data Match = Match {
    structorName  :: Text
  , matchExpr     :: Expr
  }
  deriving (Eq, Show)

type Ctx = [Expr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [(Text, Ctx)]

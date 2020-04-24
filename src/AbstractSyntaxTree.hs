{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import Data.Map (Map)

data Statement = ExprDef { name :: Text
                         , expr :: Expr
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
          | LocalExprVar Text -- Ctx-- term variables
          | GlobalExprVar Text
          | Expr :@: Expr
          | Abstr Text Expr Expr
          | TypeVar Text
          | Constructor Text
          | Destructor Text
          | Inductive Text
          | Coinductive Text
          | Rec Expr [Match]
          | Corec Expr [Match]
  deriving (Eq, Show)

data Match = Match {
    structorName  :: Text
  , exprVars      :: [Text]
  , matchExpr     :: Expr
  }
  deriving (Eq, Show)

type Ctx = [(Text, Expr)]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [(Text, Ctx)]

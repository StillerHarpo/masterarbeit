{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import Data.Map (Map)

data Expr = UnitType -- verum value
          | UnitExpr -- verum type
          | ExprVar Text -- Ctx-- term variables
          | Expr :@: Expr
          | Abstr Text Expr Expr
          | TypeVar Text
          | Constructor [Expr] Text
          | Destructror [Expr] Text
          | Inductive Ctx -- Gamma
                      [[Expr]] -- sigmas
                      [Expr] -- As
                      [Ctx] -- gamma1s
          | Coinductive Ctx -- Gamma
                        [[Expr]] -- sigmas
                        [Expr] -- As
                        [Ctx] -- gamma1s
          | Rec Expr [Expr]
          | Corec Expr [Expr]
  deriving (Eq, Show)

type Ctx = Map Text Expr

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = Map Text Expr

-- | Con/Destructor Context
-- maps Con/Destructor Names to their type names
type StrCtx = Map Text Text

data Judgment = Judgment Ctx TyCtx StrCtx Expr
  deriving (Eq, Show)

{-# language TypeOperators #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import Data.Map (Map)

data Expr = ExprVar Text -- term variables
          | VerumExpr -- verum type
          | VerumType -- verum value
          | Expr :@: Expr
          | Abstr Text Expr
          | Constructor Text -- constructor for values of inductivly defined types
          | Destructror Text
          | Inductive Text [Expr] [Expr]
          | Coinductive Text [Expr] [Expr]
          | Rec Expr [Expr]
          | Corec Expr [Expr]
          | VarC Text -- TyVar
          | Star
          | ConstructorV Ctx -- constructor for values
          | ConstructorT Ctx -- constructor for types
  deriving (Eq, Show)

newtype Ctx = Ctx (Map Text Expr)
  deriving (Eq, Show)

newtype TyCtx = TyCtx (Map Text Ctx)
  deriving (Eq, Show)

-- | Con/Destructor Context
-- maps Con/Destructor Names to their type names
newtype StrCtx = StrCtx (Map Text Text)
  deriving (Eq, Show)

data Judgement = Judgement Ctx TyCtx StrCtx Expr
  deriving (Eq, Show)

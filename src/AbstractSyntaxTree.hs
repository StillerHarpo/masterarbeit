{-# language TypeOperators #-}
{-# language RecordWildCards #-}

module AbstractSyntaxTree where

import Data.Text (Text)
import qualified Data.Text as T
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
              -- If numbers are the same the names should also be the
              -- same
              -- TODO Maybe throw error if this is not the case
              | LocalTypeVar Int (Maybe Text)
              | GlobalTypeVar Text
              | Abstr TypeExpr TypeExpr
              | In Ductive
              | Coin Ductive
  deriving (Eq)

instance Show TypeExpr where
  show UnitType   = "Unit"
  show (e1 :@ e2) = show e1 <> "@" <> show e2
  show (LocalTypeVar i Nothing) = "tmpTy" <> show i
  show (LocalTypeVar _ (Just n)) = T.unpack n
  show (GlobalTypeVar n) = T.unpack n
  -- TODO find name of variable if defined
  show (Abstr ty body) = "\\?:" <> show ty <> " -> " <> show body
  show (In d) = "mu" <> show d
  show (Coin d) = "nu" <> show d

data Ductive = Ductive { gamma :: Ctx
                       , sigmas :: [[Expr]]
                       , as :: [TypeExpr]
                       , gamma1s :: [Ctx]
                       , nameDuc :: Maybe Text
                       }

instance Show Ductive where
  show Ductive{..} = case nameDuc of
    Just t  -> T.unpack t
    Nothing -> "(" <> show gamma <> "; " <> show sigmas <> "; "
               <> show as <> "; " <> show gamma1s <> ")"

instance Eq Ductive where
  Ductive g1 s1 a1 g11 _ == Ductive g2 s2 a2 g12 _ =
    g1 == g2 && s1 == s2 && a1 == a2 && g11 == g12

data Expr = UnitExpr -- verum value
          -- If numbers are the same the names should also be the
          -- same
          -- TODO Maybe throw error if this is not the case
          | LocalExprVar Int (Maybe Text) -- Ctx-- term variables
          | GlobalExprVar Text
          | Expr :@: Expr
          | Constructor Ductive Int (Maybe Text)
          | Destructor Ductive Int (Maybe Text)
          | Rec { fromRec :: Ductive
                , toRec :: TypeExpr
                , matches :: [Expr]
                }
          | Corec { fromCorec :: TypeExpr
                  , toCorec :: Ductive
                  , matches :: [Expr]
                  }

instance Show Expr where
  show UnitExpr                   = "<>"
  show (LocalExprVar i Nothing)   = "tmp" <> show i
  show (LocalExprVar i (Just n))  = T.unpack n
  show (GlobalExprVar n)          = T.unpack n
  show (e1 :@: e2)                = show e1 <> "@" <> show e2
  show (Constructor _ _ (Just n)) = T.unpack n
  show (Constructor d i Nothing)  = "alpha_" <> show i <> "^"
                                    <> show d
  show (Destructor _ _ (Just n))  = T.unpack n
  show (Destructor d i Nothing)   = "xi_" <> show i <> "^" <> show d
  show Rec{..}                    = "rec_" <> show fromRec <> "^"
                                     <> show toRec <> show matches
  show Corec{..}                  = "rec_" <> show fromCorec <> "^"
                                     <> show toCorec <> show matches

instance Eq Expr where
  UnitExpr              == UnitExpr              = True
  (LocalExprVar i _)    == (LocalExprVar j _)    = i == j
  (GlobalExprVar n)     == (GlobalExprVar m)     = n == m
  (e1 :@: e2)           == (e3 :@: e4)           = e1 == e3 && e2 == e4
  (Constructor d1 i1 _) == (Constructor d2 i2 _) = d1 == d2 && i1 == i2
  (Destructor d1 i1 _)  == (Destructor d2 i2 _)  = d1 == d2 && i1 == i2
  (Rec fr1 tr1 ms1)     == (Rec fr2 tr2 ms2)     = fr1 == fr2 && tr1 == tr2
                                                   && and (zipWith (==) ms1 ms2)
  (Corec fc1 tc1 ms1)   == (Corec fc2 tc2 ms2)   = fc1 == fc2 && tc1 == tc2
                                                   && and (zipWith (==) ms1 ms2)
  _                     == _                     = False


type Ctx = [TypeExpr]

-- | TyCtx contains only inductive coninductive types for now
type TyCtx = [Ctx]

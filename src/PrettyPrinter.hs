{-# language RecordWildCards #-}
{-# language StandaloneDeriving #-}

module PrettyPrinter where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

import AbstractSyntaxTree

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = parens
parensIf False = id

class PrettyPresc a where
  prettyPresc :: Bool -> a -> Doc ann

prettyParens :: PrettyPresc a => a -> Doc ann
prettyParens = prettyPresc True

instance Pretty TypedExpr where
  pretty (TypedExpr expr ([], typeExpr)) = pretty expr
                                         <> pretty " :: "
                                         <> pretty typeExpr
  pretty (TypedExpr expr (ctx, typeExpr)) = pretty expr
                                         <> pretty " :: "
                                         <> pretty ctx
                                         <> pretty " -> "
                                         <> pretty typeExpr

instance PrettyPresc TypeExpr where
  prettyPresc _ UnitType = pretty "Unit"
  prettyPresc p (e1 :@ e2)  = parensIf p $ pretty e1
                                           <+> pretty "@"
                                           <+> prettyParens e2
  prettyPresc _ (LocalTypeVar i Nothing) = pretty "tmpTy" <> pretty i
  prettyPresc _ (LocalTypeVar _ (Just n)) = pretty n
  prettyPresc _ (GlobalTypeVar n par) = pretty n <+> pretty par
  -- TODO find name of variable if defined
  prettyPresc _ (Abstr ty body) = pretty "\\?:" <> pretty ty
                                                <> pretty " -> "
                                                <> prettyParens body
  prettyPresc _ (In d@Ductive{..}) = case nameDuc of
    Nothing -> pretty "mu" <> pretty d
    _       -> pretty d
  prettyPresc _ (Coin d@Ductive{..}) = case nameDuc of
    Nothing -> pretty "nu" <> pretty d
    _       -> pretty d

instance Pretty Ductive where
  pretty Ductive{..} = case nameDuc of
    Just t  -> pretty t
    Nothing -> parens . hsep $ punctuate semi [ pretty gamma
                                              , pretty sigmas
                                              , pretty as
                                              , pretty gamma1s
                                              ]

instance PrettyPresc Expr where
  prettyPresc _ UnitExpr                      = pretty "<>"
  prettyPresc _ (LocalExprVar i Nothing)      = pretty "tmp{" <> pretty i <> pretty "}"
  prettyPresc _ (LocalExprVar i (Just n))     = pretty n <> pretty "{" <> pretty i <> pretty "}"
  prettyPresc _ (GlobalExprVar n)             = pretty n
  prettyPresc p (e1 :@: e2)                   = parensIf p $ pretty e1
                                                           <+> pretty "@"
                                                           <+> prettyPresc True e2
  prettyPresc _ (Constructor _ _ (Just n))  = pretty n
  prettyPresc _ (Constructor d i Nothing)   = pretty "alpha_" <> pretty i
                                              <> pretty "^" <> pretty d
  prettyPresc _ (Destructor _ _ (Just n))   = pretty n
  prettyPresc _ (Destructor d i Nothing)    = pretty "xi_" <> pretty i
                                              <> pretty "^" <> pretty d
  prettyPresc _ Rec{..}                       = pretty "rec_" <> pretty fromRec
                                              <> pretty "^" <> pretty toRec
                                              <> pretty matches
  prettyPresc _ (WithParameters ps Rec{..})  = pretty "rec_" <> pretty fromRec
                                              <> prettyPars ps <> pretty "^"
                                              <> pretty toRec <> pretty matches
  prettyPresc _ Corec{..}                     = pretty "corec_" <> pretty fromCorec
                                              <> pretty "^" <> pretty toCorec
                                              <> pretty matches
  prettyPresc _ (WithParameters ps Corec{..}) = pretty "corec_" <> pretty fromCorec
                                              <> pretty "^" <> pretty toCorec
                                              <> prettyPars ps
                                              <> pretty matches
  prettyPresc _ (WithParameters ps e)     = pretty e <> prettyPars ps

prettyPars :: [TypeExpr] -> Doc ann
prettyPars = encloseSep langle rangle comma . map pretty

instance Pretty TypeExpr where
  pretty = prettyPresc False

instance Pretty Expr where
  pretty = prettyPresc False

instance Show TypedExpr where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Show TypeExpr where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Show Ductive where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Show Expr where
  showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

deriving instance Show Statement

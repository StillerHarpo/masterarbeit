{-# language RecordWildCards #-}
{-# language StandaloneDeriving #-}
{-# language OverloadedStrings #-}

module PrettyPrinter where


import Data.Text.Prettyprint.Doc
import Data.Text(Text)

import Lib

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
                                         <> " :: "
                                         <> pretty typeExpr
  pretty (TypedExpr expr (ctx, typeExpr)) = pretty expr
                                         <> " :: "
                                         <> encloseSep lparen rparen comma (map pretty ctx)
                                         <> " -> "
                                         <> pretty typeExpr

instance PrettyPresc TypeExpr where
  prettyPresc _ UnitType = "Unit"
  prettyPresc p (e1 :@ e2)  = parensIf p $ pretty e1
                                           <+> "@"
                                           <+> prettyParens e2
  prettyPresc _ (LocalTypeVar i "") = "?Ty" <> pretty i
  prettyPresc _ (LocalTypeVar i n) = pretty n <> "[" <> pretty i <> "]"
  prettyPresc _ (GlobalTypeVar n []) = pretty n
  prettyPresc _ (GlobalTypeVar n par) = pretty n <> angles (hsep . punctuate comma $ map pretty par)
  -- TODO find name of variable if defined
  prettyPresc _ (Abstr ty body) = "\\?:" <> pretty ty
                                         <> " -> "
                                         <> prettyParens body
  prettyPresc _ (In d@Ductive{..}) = case nameDuc of
    "???" -> "mu" <> pretty d
    _       -> pretty d
  prettyPresc _ (Coin d@Ductive{..}) = case nameDuc of
    "???" -> "nu" <> pretty d
    _       -> pretty d

instance Pretty Ductive where
  pretty Ductive{..} = case nameDuc of
    "???" -> parens . hsep $ punctuate semi [ pretty gamma
                                              , pretty sigmas
                                              , pretty as
                                              , pretty gamma1s
                                              ]
    t     -> pretty t

instance PrettyPresc Expr where
  prettyPresc _ UnitExpr                       = "<>"
  prettyPresc _ (LocalExprVar i "")            = "?" <> pretty i
  prettyPresc _ (LocalExprVar i n)             = pretty n <> "{" <> pretty i <> "}"
  prettyPresc _ (GlobalExprVar n)              = pretty n
  prettyPresc p (e1 :@: e2)                    =
    parensIf p $ pretty e1 <+> "@" <+> prettyPresc True e2
  prettyPresc _ (Constructor d@Ductive{..}  i) =
    case strNames !!? i of
      Just n  -> pretty n
      Nothing -> "alpha_" <> pretty i <> "^" <> pretty d
  prettyPresc _ (Destructor d@Ductive{..} i)   =
    case strNames !!? i of
      Just n  -> pretty n
      Nothing -> "xi_" <> pretty i <> "^" <> pretty d
  prettyPresc _ Rec{..}                        =
    let Ductive{..} = fromRec
    in parens $ nest 2 $ vsep $ ("rec" <+> pretty fromRec
                        <+> "to" <+> pretty toRec <+> "where")
                       : prettyMatches strNames gamma1s matches
  prettyPresc _ (WithParameters ps Rec{..})    =
    let Ductive{..} = fromRec
    in parens $ nest 2 $ vsep $ ("rec" <+> pretty fromRec <> prettyPars ps
                         <+> "to" <+> pretty toRec <+> "where")
                       : prettyMatches strNames gamma1s matches
  prettyPresc _ Corec{..}                      =
    let Ductive{..} = toCorec
    in parens $ nest 2 $ vsep $ ("corec" <+> pretty fromCorec
                       <+> "to" <+> pretty toCorec <+> "where")
                       : prettyMatches strNames gamma1s matches
  prettyPresc _ (WithParameters ps Corec{..})   =
    let Ductive{..} = toCorec
    in parens $ nest 2 $ vsep $ ("corec" <+> pretty fromCorec <+> "to"
                        <+> pretty toCorec <> prettyPars ps <+> "where")
                        : prettyMatches strNames gamma1s matches
  prettyPresc _ (WithParameters ps e)           = pretty e
                                                  <> prettyPars ps

prettyMatches :: [Text] -- Constructors
              -> [Ctx] --- Gamma1s
              -> [Expr] -- Matches
              -> [Doc ann]
prettyMatches = zipWith3 prettyMatch
  where
    prettyMatch :: Text -> Ctx -> Expr -> Doc ann
    prettyMatch str ctx match = pretty str
                                <+> hsep ["x" <> pretty i | i <- [0..(length ctx)]]
                                <+> "=" <+> pretty match

prettyPars :: [TypeExpr] -> Doc ann
prettyPars = encloseSep langle rangle comma . map pretty

instance Pretty TypeExpr where
  pretty = prettyPresc False

instance Pretty Expr where
  pretty = prettyPresc False

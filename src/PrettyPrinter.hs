{-# language RecordWildCards #-}
{-# language StandaloneDeriving #-}
{-# language OverloadedStrings #-}

module PrettyPrinter where


import Data.Text.Prettyprint.Doc
import Data.Text                 (Text)

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
  pretty (TypedExpr expr ([], typeExpr)) =
    pretty expr <> " :: " <> pretty typeExpr
  pretty (TypedExpr expr (ctx, typeExpr)) =
    pretty expr <> " :: "
                <> encloseSep lparen rparen comma (map pretty ctx)
                <> " -> "
                <> pretty typeExpr

instance PrettyPresc TypeExpr where
  prettyPresc _ UnitType              =
    "Unit"
  prettyPresc p (e1 :@ e2)            =
    parensIf p $ pretty e1 <+> "@" <+> prettyParens e2
  prettyPresc _ (LocalTypeVar i "")   =
    "?Ty" <> pretty i
  prettyPresc _ (LocalTypeVar i n)    =
    pretty n <> "[" <> pretty i <> "]"
  prettyPresc _ (Parameter i "")   =
    "?Par" <> pretty i
  prettyPresc _ (Parameter i n)       =
    pretty n <> "[" <> pretty i <> "]"
  prettyPresc _ (GlobalTypeVar n [])  =
    pretty n
  prettyPresc _ (GlobalTypeVar n par) =
    pretty n <> angles (hsep . punctuate comma $ map pretty par)
  -- TODO find name of variable if defined
  prettyPresc _ (Abstr ty body)       =
    "\\?:" <> pretty ty <> " -> " <> prettyParens body
  prettyPresc _ (In d@Ductive{..})    =
    case nameDuc of
      "???"   -> "mu" <> pretty d
      _       -> pretty d
  prettyPresc _ (Coin d@Ductive{..})  =
    case nameDuc of
      "???"   -> "nu" <> pretty d
      _       -> pretty d

instance Pretty Ductive where
  pretty Ductive{..} = pretty nameDuc
  {- case nameDuc of
    "???" -> parens . hsep $ punctuate semi [ pretty gamma
                                            , pretty sigmas
                                            , pretty as
                                            , pretty gamma1s
                                            ]
    t     -> pretty t -}

instance PrettyPresc Expr where
  prettyPresc _ UnitExpr                       =
    "<>"
  prettyPresc _ (LocalExprVar i "")            =
    "?" <> pretty i
  prettyPresc _ (LocalExprVar i n)             =
    pretty n <> "{" <> pretty i <> "}"
  prettyPresc _ (GlobalExprVar n [] [])        =
    pretty n
  prettyPresc _ (GlobalExprVar n tyPars [])    =
    pretty n <> angles (hsep . punctuate comma $ map pretty tyPars)
  prettyPresc _ (GlobalExprVar n [] exprPars)  =
    pretty n <> parens (hsep . punctuate comma $ map pretty exprPars)
  prettyPresc _ (GlobalExprVar n tyPars exprPars) =
    pretty n <> angles (hsep . punctuate comma $ map pretty tyPars)
             <> parens (hsep . punctuate comma $ map pretty exprPars)
  prettyPresc p (e1 :@: e2)                    =
    parensIf p $ pretty e1 <+> "@" <+> prettyPresc True e2
  prettyPresc _ (Constructor d@Ductive{..}  i) =
    case strName (strDefs !! i) of
      "" -> "alpha_" <> pretty i <> "^" <> pretty d
      n  -> pretty n
  prettyPresc _ (Destructor d@Ductive{..} i)   =
    case strName (strDefs !! i) of
      "" -> "xi_" <> pretty i <> "^" <> pretty d
      n  -> pretty n
  prettyPresc _ Rec{..}                        =
    let Ductive{..} = fromRec
    in parens $ nest 2 $ vsep $ ("rec" <+> pretty fromRec
                                       <+> "to"
                                       <+> pretty toRec
                                       <+> "where")
                       : prettyMatches strDefs matches
  prettyPresc _ (WithParameters ps Rec{..})    =
    let Ductive{..} = fromRec
    in parens $ nest 2 $ vsep $ ("rec" <+> pretty fromRec
                                       <> prettyPars ps
                                       <+> "to"
                                       <+> pretty toRec
                                       <+> "where")
                       : prettyMatches strDefs matches
  prettyPresc _ Corec{..}                      =
    let Ductive{..} = toCorec
    in parens $ nest 2 $ vsep $ ("corec" <+> pretty fromCorec
                                         <+> "to"
                                         <+> pretty toCorec
                                         <+> "where")
                       : prettyMatches strDefs matches
  prettyPresc _ (WithParameters ps Corec{..})   =
    let Ductive{..} = toCorec
    in parens $ nest 2 $ vsep $ ("corec" <+> pretty fromCorec
                                         <+> "to"
                                         <+> pretty toCorec
                                         <> prettyPars ps
                                         <+> "where")
                        : prettyMatches strDefs matches
  prettyPresc _ (WithParameters ps e)           =
    pretty e <> prettyPars ps

prettyMatches :: [StrDef]
              -> [Expr] -- Matches
              -> [Doc ann]
prettyMatches = zipWith prettyMatch
  where
    prettyMatch :: StrDef -> Expr -> Doc ann
    prettyMatch StrDef{..} match =
      pretty strName
      <+> hsep ["x" <> pretty i | i <- [0..(length gamma1)]]
      <+> "="
      <+> pretty match

prettyPars :: [TypeExpr] -> Doc ann
prettyPars = encloseSep langle rangle comma . map pretty

instance Pretty TypeExpr where
  pretty = prettyPresc False

instance Pretty Expr where
  pretty = prettyPresc False

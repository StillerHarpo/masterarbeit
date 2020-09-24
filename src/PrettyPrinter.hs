{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module PrettyPrinter where

import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc

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
  prettyPresc _ (LocalTypeVar i _ "")  =
    "?Ty" <> pretty i
  prettyPresc _ (LocalTypeVar i _ n)   =
    pretty n <> "[" <> pretty i <> "]"
  prettyPresc _ (Parameter i _ "")   =
    "?Par" <> pretty i
  prettyPresc _ (Parameter i _ n)       =
    pretty n <> "[" <> pretty i <> "]"
  prettyPresc _ (GlobalTypeVar n [])  =
    pretty n
  prettyPresc _ (GlobalTypeVar n par) =
    pretty n <> angles (hsep . punctuate comma $ map pretty par)
  -- TODO find name of variable if defined
  prettyPresc _ (Abstr n ty body)       =
    "(" <> pretty n <+> ":" <+> pretty ty <> f body
      where f (Abstr n ty body) = "," <> pretty n <+> pretty ty <> f body
            f body = ")." <> prettyParens body
  prettyPresc _ Ductive{..}           =
     pretty openDuctive <> prettyPars parametersTyExpr

instance Pretty OpenDuctive where
  pretty OpenDuctive{..} = pretty nameDuc

instance PrettyPresc Expr where
  prettyPresc _ UnitExpr                       =
    "()"
  prettyPresc _ (LocalExprVar i _ "")          =
    "?" <> pretty i
  prettyPresc _ (LocalExprVar i _ n)           =
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
  prettyPresc _ Structor{..}                   =
    pretty (strName $ strDefs ductive !! num) <> prettyPars parameters
  prettyPresc _ Iter{..}                       =
    let OpenDuctive{..} = ductive
    in parens $ nest 2 $ vsep
       $ (case inOrCoin of
           IsIn -> "rec" <+> pretty ductive
                          <> prettyPars parameters
                          <+> "to"
                          <+> pretty motive
                          <+> "where"
           IsCoin -> "corec" <+> pretty motive
                              <+> "to"
                              <+> pretty ductive
                              <> prettyPars parameters
                              <+> "where")
         : prettyMatches strDefs matches

prettyMatches :: [StrDef]
              -> [([Text],Expr)] -- Matches
              -> [Doc ann]
prettyMatches = zipWith prettyMatch
  where
    prettyMatch :: StrDef -> ([Text], Expr) -> Doc ann
    prettyMatch StrDef{..} (vars, match) =
      pretty strName
      <+> hsep (map pretty vars)
      <+> hsep ["x" <> pretty i | i <- [0..(length gamma1 - length vars)]]
      <+> "="
      <+> pretty match

prettyPars :: [TypeExpr] -> Doc ann
prettyPars [] = ""
prettyPars xs = encloseSep langle rangle comma $ map pretty xs

instance Pretty TypeExpr where
  pretty = prettyPresc False

instance Pretty Expr where
  pretty = prettyPresc False

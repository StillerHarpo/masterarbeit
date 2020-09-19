{-# language OverloadedStrings#-}
module Vector.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Nat.Definition
import Nat.Examples

pairD, pairDR :: Text
pairD = T.unlines
  [ "codata Pair<A : Set, B : (n : Nat) -> Set> : (n : Nat) -> Set where"
  , "  First : (n : Nat) -> Pair n -> A"
  , "  Second : (n : Nat) -> Pair n -> B @ n"]
pairDR = T.unlines [natD, pairD]

pairDuc :: OpenDuctive
pairDuc = OpenDuctive { gamma = [GlobalTypeVar "Nat" []]
                      , inOrCoin = IsCoin
                      , parameterCtx = [[],[GlobalTypeVar "Nat" []]]
                      , strDefs = [ StrDef { sigma = [LocalExprVar 0 False "n"]
                                           , a = Parameter 1 False "A"
                                           , gamma1 = [GlobalTypeVar "Nat" []]
                                           , strName = "First"}
                                  , StrDef { sigma = [LocalExprVar 0 False "n"]
                                           , a = Parameter 0 False "B"
                                                 :@ LocalExprVar 0 False "n"
                                           , gamma1 = [GlobalTypeVar "Nat" []]
                                           , strName = "Second"}]
                      , nameDuc = "Pair" }

pairExpr :: TypeExpr -> TypeExpr -> TypeExpr
pairExpr x y = Ductive { openDuctive = pairDuc
                       , parametersTyExpr = [x,y]}

fstExpr :: TypeExpr -> TypeExpr -> Expr
fstExpr x y = Structor { ductive = pairDuc
                       , parameters = [x,y]
                       , num = 0}

sndExpr :: TypeExpr -> TypeExpr -> Expr
sndExpr x y = Structor { ductive = pairDuc
                       , parameters = [x,y]
                       , num = 1}
pairTest :: Spec
pairTest = do
  it "parses pair definition" $
    shouldParseWithDefs [natD] pairD
      [ TypeDef pairDuc ]
  it "kind checks pair definition" $
    shouldKindCheckWithDefs [natD] (pairExpr UnitType
                                             (Abstr (GlobalTypeVar "Nat" [])
                                                     UnitType))
      [ GlobalTypeVar "Nat" []]

vecD, vecDR :: Text
vecD = T.unlines
  [ "data Vec<A : Set> : (n:Nat) -> Set where"
  , "  Nil : Unit -> Vec zero"
  , "  Cons : (n : Nat) -> Pair<A, Vec> @ n -> Vec (Suc @ n)"
  ]
vecDR = T.unlines [pairDR, vecD]

vecDuc :: OpenDuctive
vecDuc =
  OpenDuctive { gamma = [GlobalTypeVar "Nat" []]
              , inOrCoin = IsIn
              , parameterCtx = [[]]
              , strDefs =
                  [ StrDef { sigma = [GlobalExprVar "zero" [] []]
                           , a = UnitType
                           , gamma1 = []
                           , strName = "Nil" }
                  , StrDef { sigma = [Structor { ductive = natDuc
                                               , parameters = []
                                               , num = 1}
                                      :@: LocalExprVar 0 False "n"]
                           , a = GlobalTypeVar "Pair" [ Parameter 0 False "A"
                                                      , LocalTypeVar 0 False "Vec"]
                                 :@ LocalExprVar 0 False "n"
                           , gamma1 = [GlobalTypeVar "Nat" []]
                           , strName = "Cons"}]
              , nameDuc = "Vec"}

vecExpr :: TypeExpr -> TypeExpr
vecExpr x = Ductive { openDuctive = vecDuc
                     , parametersTyExpr = [x]}

consExpr :: TypeExpr -> Expr
consExpr x = Structor { ductive = vecDuc
                      , parameters = [x]
                      , num = 1}

nilExpr :: TypeExpr -> Expr
nilExpr x = Structor { ductive = vecDuc
                     , parameters = [x]
                     , num = 0} :@: UnitExpr

vecTest :: Spec
vecTest = do
  it "Parses the definition of vector" $
    shouldParseWithDefs [pairDR, zeroD] vecD
      [ TypeDef vecDuc ]
  it "kind checks vector dfinition" $
    shouldKindCheckWithDefs [pairDR, zeroD] (vecExpr UnitType)
      [ GlobalTypeVar "Nat" []]

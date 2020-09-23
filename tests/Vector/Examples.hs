{-# language OverloadedStrings#-}
module Vector.Examples where

import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Hspec
import           Test.Hspec.Hedgehog    (forAll, hedgehog)

import qualified Data.Text              as T
import           Data.Text              (Text)

import           AbstractSyntaxTree

import           Lib
import           Nat.Definition
import           Nat.Examples
import           Vector.Definition

mkPairD, mkPairDR :: Text
mkPairD = T.unlines
  -- TODO make block parsing work in nested cases
  [ "mkPair<A : Set, B : (n: Nat) -> Set>(n : Nat, x : A, y : B @ n) ="
  , "  ((corec<A,B> B to Pair where"
  , "     { First n p = x"
  , "     ; Second n p = p}) @ n @ y)"]
mkPairDR = T.unlines [pairDR, mkPairD]

mkPairStmt :: Statement
mkPairStmt =
  ExprDef { name = "mkPair"
          , tyParameterCtx = [[],[GlobalTypeVar "Nat" []]]
          , exprParameterCtx = [ GlobalTypeVar "Nat" []
                               , Parameter 1 False "A"
                               , Parameter 0 False "B"
                                 :@ LocalExprVar 1 False "n"]
          , expr = Iter { ductive = pairDuc
                        , parameters = [ Parameter 1 False "A"
                                       , Parameter 0 False "B"]
                        , motive = Parameter 0 False "B"
                        , matches = [ ( ["n", "p"]
                                      , LocalExprVar 3 False "x")
                                    , ( ["n", "p"]
                                      , LocalExprVar 0 False "p")]}
                   :@: LocalExprVar 2 False "n"
                   :@: LocalExprVar 0 False "y"
          , ty = Nothing }

mkPairTests :: Spec
mkPairTests = do
  it "parses mkPair" $
    shouldParseWithDefs [pairDR] mkPairD
      [ mkPairStmt ]
  it "type checks mkPair" $
    shouldCheckStmtWithDefs [pairDR] mkPairStmt
      ([], pairExpr (Parameter 1 False "A") (Parameter 0 False "B")
           :@ LocalExprVar 2 False "n")

nilD, nilDR :: Text
nilD = "nil<A : Set> = Nil<A> @ ()"
nilDR = T.unlines [vecDR, nilD]

nilStmt :: Statement
nilStmt =
  ExprDef { name = "nil"
          , tyParameterCtx = [[]]
          , exprParameterCtx = []
          , expr = nilExpr (Parameter 0 False "A")
          , ty = Nothing }

nilTests :: Spec
nilTests = do
  it "parses nil" $
    shouldParseWithDefs [vecDR] nilD
      [ nilStmt ]
  it "type checks nil" $
    shouldCheckStmtWithDefs [vecDR] nilStmt
      ([], vecExprI (Parameter 0 False "A") :@ zeroExpr)

consD, consDR :: Text
consD = T.unlines
  [ "cons<A : Set>(n : Nat, x : A, xs : Vec<A> @ n) ="
  , "  Cons<A> @ n @ mkPair<A, Vec<A>>(n,x,xs)" ]
consDR = T.unlines [mkPairDR, zeroD, vecD, consD]

consStmt :: Statement
consStmt =
  ExprDef { name = "cons"
          , tyParameterCtx = [[]]
          , exprParameterCtx = [ GlobalTypeVar "Nat" []
                               , Parameter 0 False "A"
                               , GlobalTypeVar "Vec" [Parameter 0 False "A"]
                                 :@ LocalExprVar 1 False "n"]
          , expr = consExpr (Parameter 0 False "A")
                   :@: LocalExprVar 2 False "n"
                   :@: GlobalExprVar "mkPair" [ Parameter 0 False "A"
                                              , GlobalTypeVar "Vec" [Parameter 0 False "A"]]
                                              [ LocalExprVar 2 False "n"
                                              , LocalExprVar 1 False "x"
                                              , LocalExprVar 0 False "xs" ]
          , ty = Nothing }

consTests :: Spec
consTests = do
  it "parses cons" $
    shouldParseWithDefs [mkPairDR, zeroD, vecD] consD
      [ consStmt ]
  it "type checks cons" $
    shouldCheckStmtWithDefs [mkPairDR, zeroD, vecD] consStmt
      ([], vecExprI (Parameter 0 False "A")
           :@ (sucExpr :@: LocalExprVar 2 False "n"))

vecEx1D, vecEx1DR, vecEx2D, vecEx2DR :: Text
vecEx3D, vecEx3DR, vecEx4D, vecEx4DR :: Text
vecEx5D, vecEx5DR :: Text
vecEx1D = "vec1 = nil<Unit>"
vecEx1DR = T.unlines [vecDR, nilD, vecEx1D]
vecEx2D = "vec2 = cons<Unit>(zero,(),vec1)"
vecEx2DR = T.unlines [vecEx1DR, consD, vecEx2D]
vecEx3D = "vec3 = nil<Nat>"
vecEx3DR = T.unlines [vecDR, nilD, vecEx3D]
vecEx4D = "vec4 = cons<Nat>(zero,one,vec3)"
vecEx4DR = T.unlines [vecEx3DR, oneD, mkPairD, consD, vecEx4D]
vecEx5D = "vec5 = cons<Nat>(one, two, vec4)"
vecEx5DR = T.unlines [vecEx4DR, twoD, vecEx5D]

vecEx1Expr, vecEx2Expr, vecEx3Expr, vecEx4Expr, vecEx5Expr :: Expr
vecEx1Expr = GlobalExprVar "nil" [UnitType] []
vecEx2Expr = GlobalExprVar "cons"
                           [UnitType]
                           [ GlobalExprVar "zero" [] []
                           , UnitExpr
                           , GlobalExprVar "vec1" [] []]
vecEx3Expr = GlobalExprVar "nil" [GlobalTypeVar "Nat" []] []
vecEx4Expr = GlobalExprVar "cons"
                           [GlobalTypeVar "Nat" []]
                           [ GlobalExprVar "zero" [] []
                           , GlobalExprVar "one" [] []
                           , GlobalExprVar "vec3" [] []]
vecEx5Expr = GlobalExprVar "cons"
                           [GlobalTypeVar "Nat" []]
                           [ GlobalExprVar "one" [] []
                           , GlobalExprVar "two" [] []
                           , GlobalExprVar "vec4" [] []]

genVecExpr :: TypeExpr -> [Expr] -> Expr
genVecExpr ty [] = GlobalExprVar "nil" [ty] []
genVecExpr ty (x:xs) = GlobalExprVar "cons"
                                     [ty]
                                     [ genNatExpr $ length xs
                                     , x
                                     , genVecExpr ty xs ]

vecExTests :: Spec
vecExTests = do
  it "Parses a empty vec of units" $
    shouldParseWithDefs [vecDR, nilD] vecEx1D
      [ ExprDef { name = "vec1"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = vecEx1Expr
                , ty = Nothing}]
  it "Type checks a empty vec of units to Vec<Unit> @ 0" $
    shouldCheckWithDefs [vecDR, nilD] vecEx1Expr
      ([], vecExprI UnitType :@ zeroExpr)
  it "Parses a vec with one unit" $
    shouldParseWithDefs [vecEx1DR, mkPairD, consD] vecEx2D
      [ ExprDef { name = "vec2"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = vecEx2Expr
                , ty = Nothing}]
  it "Type checks a vec with one unit to Vec<Unit> @ 1" $
    shouldCheckWithDefs [vecEx1DR, mkPairD, consD] vecEx2Expr
      ([], vecExprI UnitType :@ oneExpr)
  it "Parses a empty vec of nats" $
    shouldParseWithDefs [vecDR, nilD] vecEx3D
      [ ExprDef { name = "vec3"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = vecEx3Expr
                , ty = Nothing}]
  it "Type checks a empty vec of nats to Vec<Nat> @ 0" $
    shouldCheckWithDefs [vecDR, nilD] vecEx3Expr
      ([], vecExprI (GlobalTypeVar "Nat" []) :@ zeroExpr)
  it "Parses a vec with one number one" $
    shouldParseWithDefs [vecEx3DR, oneD, mkPairD, consD] vecEx4D
      [ ExprDef { name = "vec4"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = vecEx4Expr
                , ty = Nothing}]
  it "Type checks a vec with one number one to Vec<Nat> @ 1" $
    shouldCheckWithDefs [vecEx3DR, oneD, mkPairD, consD] vecEx4Expr
      ([], vecExprI (GlobalTypeVar "Nat" []) :@ oneExpr)
  it "Parses a vec with numbers one and two" $
    shouldParseWithDefs [vecEx4DR, twoD] vecEx5D
      [ ExprDef { name = "vec5"
                , tyParameterCtx = []
                , exprParameterCtx = []
                , expr = vecEx5Expr
                , ty = Nothing}]
  it "Type checks a vec with umber one two to Vec<Nat> @ 2" $
    shouldCheckWithDefs [vecEx4DR, twoD] vecEx5Expr
      ([], vecExprI (GlobalTypeVar "Nat" []) :@ twoExpr)
  it "Type checks any vec of nats to vec of nats" $ hedgehog $ do
      xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.integral (Range.linear 0 100))
      shouldCheckWithDefsP [nilDR, mkPairD, consD] (genVecExpr (GlobalTypeVar "Nat" []) (map genNatExpr xs))
        ([], vecExprI (GlobalTypeVar "Nat" []) :@ genNatExpr (length xs))



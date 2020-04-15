{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Data.Map as Map

import Control.Monad.State.Strict

import qualified Data.Text as T

import Parser
import AbstractSyntaxTree

main :: IO ()
main = hspec $ do
  describe "Parser works" $ do
    let parserState = ParserState { _scLineFold = sc
                                  , _ctx = Map.empty
                                  , _tyCtx = Map.empty
                                  , _strCtx = Map.empty }
    let parseExprS = evalStateT parseExpr parserState
    it "parses the unit expression" $
      parse parseExprS "" "()" `shouldParse` UnitExpr
    it "parses the unit type" $
      parse parseExprS "" "Unit" `shouldParse` UnitType
    it "parses a expression variable" $
      parse parseExprS "" "x1" `shouldParse`  ExprVar "x1"
    it "parses a expression variable" $
      parse parseExprS "" "C1" `shouldParse`  TypeVar "C1"
    it "parses a application" $
      parse parseExprS "" "x @ y"
      `shouldParse`
      (ExprVar "x" :@: ExprVar "y")
    it "parses a application 2" $
      parse parseExprS "" "A @ x"
      `shouldParse`
      (TypeVar "A" :@: ExprVar "x")
    it "parses a application with brackets" $
      parse parseExprS "" "(A @ x)"
      `shouldParse`
      (TypeVar "A" :@: ExprVar "x")
    it "parses a abstraction" $
      parse parseExprS "" "(x:Unit).()"
      `shouldParse`
      Abstr "x" UnitType UnitExpr
    it "parses a abstraction with multiple arguments" $
      parse parseExprS "" "(x:Unit,y:Unit).()"
      `shouldParse`
      Abstr "x" UnitType (Abstr "y" UnitType UnitExpr)
    it "parse a application left associative" $
      parse parseExprS "" "x @ z @ y"
      `shouldParse`
      (ExprVar "x" :@: ExprVar "z" :@: ExprVar "y")
    it "parse a expression with brackets" $
      parse parseExprS "" "x @ (z @ y)"
      `shouldParse`
      (ExprVar "x" :@: (ExprVar "z" :@: ExprVar "y"))
    let parseStatementS =  (\parserState -> (_tyCtx parserState, _strCtx parserState))
         <$> execStateT parseStatement parserState
    let parseDefinitionS = _ctx <$> execStateT parseStatement parserState
    it "parses definitions" $
      parse parseDefinitionS "" "x = ()"
      `shouldParse`
      Map.singleton "x" UnitExpr
    it "parses data" $
      parse parseStatementS "" "data A : Set where { C1 : A -> A }"
      `shouldParse`
      ( Map.singleton "A" (Inductive Map.empty [[]] [TypeVar "A"] [Map.empty])
      , Map.singleton "C1" "A")
    it "parses codata" $
      parse parseStatementS "" "codata A : Set where { C1 : A -> A }"
      `shouldParse`
      ( Map.singleton "A" (Coinductive Map.empty [[]] [TypeVar "A"] [Map.empty])
      , Map.singleton "C1" "A")
    let parseMatchS = evalStateT parseMatch parserState
    it "parses match" $
      parse parseMatchS "" "A x = ()" `shouldParse` Match "A" [] ["x"] UnitExpr
    it "parses rec" $
      parse parseExprS "" "rec Unit where { A x = () }"
      `shouldParse`
      Rec UnitType [Match "A" [] ["x"] UnitExpr]
    it "parses corec" $
      parse parseExprS "" "corec Unit where { A x = ()}"
      `shouldParse`
      Corec UnitType [Match "A" [] ["x"] UnitExpr]
    it "parses rec with type indexes" $
      parse parseExprS "" "rec Unit where { A<B,Unit> x = () }"
      `shouldParse`
      Rec UnitType [Match "A" [TypeVar "B", UnitType] ["x"] UnitExpr]
    it "parses judgments" $
      parse parseJudgment "" "y = (); y"
      `shouldParse`
      Judgment (Map.fromList [("y",UnitExpr)])
               Map.empty
               Map.empty
               (ExprVar "y")
    it "parses a statement with line folding" $
      parse parseDefinitionS "" (T.unlines [ "x ="
                                           , "  ()"
                                           , "   @"
                                           , "    ()"])
      `shouldParse`
       Map.singleton "x" (UnitExpr :@: UnitExpr)
    it "parses indented data definition" $
      parse parseStatementS "" (T.unlines [ "data A : (y:B) -> Set where"
                                          , "  C1 : A -> A"
                                          , "  C2 : (A @ y) -> A"])
      `shouldParse`
      ( Map.singleton "A" (Inductive (Map.singleton "y" (TypeVar "B"))
                                     [[],[]]
                                     [TypeVar "A" :@: ExprVar "y", TypeVar "A"]
                                     [Map.fromList [], Map.fromList []])
      , Map.fromList [("C2","A"), ("C1","A")])
    it "parses a program" $
      parse (parseJudgment <* scn <* eof) "" (T.unlines [ "x = ()"
                                        , "data A : (y:B) -> Set where"
                                        , "  C1 : A -> A"
                                        , "  C2 : (A @ y) -> A"
                                        , "x @ x"])
      `shouldParse`
      Judgment (Map.singleton "x" UnitExpr)
               (Map.singleton "A"
                              (Inductive (Map.singleton "y" (TypeVar "B"))
                                          [[],[]]
                                          [TypeVar "A" :@: ExprVar "y", TypeVar "A"]
                                          [Map.fromList [], Map.fromList []]))
                (Map.fromList [("C1","A"),("C2","A")])
                (ExprVar "x" :@: ExprVar "x")


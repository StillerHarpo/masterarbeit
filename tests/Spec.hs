{-# language OverloadedStrings#-}
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Data.Map as Map

import Control.Monad.State.Strict
import Lens.Micro.Platform

import qualified Data.Text as T

import Parser
import AbstractSyntaxTree

main :: IO ()
main = hspec $ do
  describe "Parser works" $ do
    let parserState = ParserState { _scLineFold = sc
                                  , _ctx = Map.fromList [ ("x", UnitExpr)
                                                        , ("y", UnitExpr)
                                                        , ("z", UnitExpr)]
                                  , _tyCtx = Map.fromList [ ("A", UnitType)
                                                          , ("B", UnitType)]
                                  , _strCtx = Map.empty }
    let parseExprS = evalStateT parseExpr parserState
    it "parses the unit expression" $
      parse parseExprS "" "()" `shouldParse` UnitExpr
    it "parses the unit type" $
      parse parseExprS "" "Unit" `shouldParse` UnitType
    it "parses a expression variable" $
      parse parseExprS "" "x" `shouldParse`  ExprVar "x"
    it "parses a type variable" $
      parse parseExprS "" "A" `shouldParse`  TypeVar "A" []
    it "parses a application" $
      parse parseExprS "" "x @ y"
      `shouldParse`
      (ExprVar "x" :@: ExprVar "y")
    it "parses a application 2" $
      parse parseExprS "" "A @ x"
      `shouldParse`
      (TypeVar "A" [] :@: ExprVar "x")
    it "parses a application with brackets" $
      parse parseExprS "" "(A @ x)"
      `shouldParse`
      (TypeVar "A" [] :@: ExprVar "x")
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
         <$> execStateT parseStatement (set tyCtx Map.empty parserState)
    let parseDefinitionS = _ctx <$> execStateT parseStatement (set ctx Map.empty parserState)
    it "parses definitions" $
      parse parseDefinitionS "" "x = ()"
      `shouldParse`
      Map.singleton "x" UnitExpr
    it "parses data" $
      parse parseStatementS "" "data C : Set where { C1 : C -> C }"
      `shouldParse`
      ( Map.singleton "C" (Inductive Map.empty [[]] [TypeVar "C" []] [Map.empty])
      , Map.singleton "C1" "C")
    it "parses codata" $
      parse parseStatementS "" "codata C : Set where { C1 : C -> C }"
      `shouldParse`
      ( Map.singleton "C" (Coinductive Map.empty [[]] [TypeVar "C" []] [Map.empty])
      , Map.singleton "C1" "C")
    -- TODO Tests for constructors and destructors
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
      Rec UnitType [Match "A" [TypeVar "B" [], UnitType] ["x"] UnitExpr]
    it "parses judgments" $
      parse parseJudgment "" "y = (); y"
      `shouldParse`
      Judgment (Map.fromList [("y",UnitExpr)])
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
      parse parseStatementS "" (T.unlines [ "data C : (y:Unit) -> Set where"
                                          , "  C1 : C -> C"
                                          , "  C2 : (C @ y) -> C"])
      `shouldParse`
      ( Map.singleton "C" (Inductive (Map.singleton "y" UnitType)
                                     [[],[]]
                                     [TypeVar "C" [] :@: ExprVar "y", TypeVar "C" []]
                                     [Map.fromList [], Map.fromList []])
      , Map.fromList [("C2","C"), ("C1","C")])
    it "parses a program" $
      parse (parseJudgment <* scn <* eof) "" (T.unlines [ "x = ()"
                                        , "data A : (y:Unit) -> Set where"
                                        , "  C1 : A -> A"
                                        , "  C2 : (A @ y) -> A"
                                        , "x @ x"])
      `shouldParse`
      Judgment (Map.singleton "x" UnitExpr)
               (Map.singleton "A"
                              (Inductive (Map.singleton "y" UnitType)
                                          [[],[]]
                                          [TypeVar "A" [] :@: ExprVar "y", TypeVar "A" []]
                                          [Map.fromList [], Map.fromList []]))
                (ExprVar "x" :@: ExprVar "x")

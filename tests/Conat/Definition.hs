{-# language OverloadedStrings#-}
module Conat.Definition where

import Test.Hspec

import qualified Data.Text as T
import Data.Text(Text)

import AbstractSyntaxTree

import Lib
import Maybe.Definition

conatD :: Text
conatD = T.unlines
  [ "codata Conat : Set where"
  , "  Prev  : Conat -> Maybe<Conat>"
  ]

conatDR :: Text
conatDR = T.unlines [maybeD, conatD]

conatDuc :: OpenDuctive
conatDuc = OpenDuctive { gamma = []
                       , inOrCoin = IsCoin
                       , parameterCtx = []
                       , strDefs = [ StrDef { sigma = []
                                            , a = GlobalTypeVar "Maybe" [LocalTypeVar 0 "Conat"]
                                            , gamma1 = []
                                            , strName = "Prev"}]
                       , nameDuc = "Conat" }

conatExpr :: TypeExpr
conatExpr = Ductive { parametersTyExpr = []
                    , openDuctive = conatDuc}

prevExpr :: Expr
prevExpr = Structor { ductive = conatDuc
                    , parameters = []
                    , num = 0}

conatTest :: Spec
conatTest =
  it "Parses the definition of Conat" $
    shouldParseWithDefs [maybeD] conatD
      [ TypeDef conatDuc ]



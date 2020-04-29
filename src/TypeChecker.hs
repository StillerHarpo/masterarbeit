{-# language OverloadedStrings #-}
{-# language TupleSections #-}

module TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Text (Text)

import AbstractSyntaxTree


type TI = ExceptT Text (Reader ())

type Type = Ctx

checkTyCtx :: TyCtx -> TI ()
checkTyCtx [] = pure ()
checkTyCtx ((_,ctx):tyCtx) = checkCtx ctx >> checkTyCtx tyCtx

checkCtx :: Ctx -> TI ()
checkCtx [] = pure ()
checkCtx (typ:ctx) = checkType [] ctx typ []

checkType :: TyCtx -> Ctx -> Expr -> Type -> TI ()
checkType = undefined

inferType :: TyCtx -> Ctx -> Expr -> TI Type
inferType = undefined

checkTerm :: Ctx -> Expr -> (Ctx,Expr) -> TI ()
checkTerm = undefined

inferTerm :: Ctx -> Expr -> TI (Ctx,Expr)
inferTerm _ UnitExpr = pure ([],UnitType)
inferTerm ctx (LocalExprVar x) = ([],) <$> lookupTI x ctx
inferTerm _   UnitType = throwError "This is not a type"
inferTerm _   Abstr{} = throwError "This is not a type"
inferTerm _   TypeVar{} = throwError "This is not a type"
inferTerm _   Inductive{} = throwError "This is not a type"
inferTerm _   Coinductive{} = throwError "This is not a type"


lookupTI :: Int -> [a] -> TI a
lookupTI _ []     = throwError "Variable not defined"
lookupTI 0 (x:_)  = pure x
lookupTI n (x:xs) = lookupTI (n-1) xs

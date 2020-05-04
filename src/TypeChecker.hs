{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}

module TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Lens.Micro.Platform

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (fromJust)

import AbstractSyntaxTree


data ContextTI = ContextTI { _ctx :: Ctx
                           , _tyCtx :: TyCtx
                           , _defCtx :: [Statement]
                           , _strCtx :: StrCtx
                           }
$(makeLenses ''ContextTI)

type TI = ExceptT Text (Reader ContextTI)

runTI :: TI a -> ContextTI -> Either Text a
runTI ti = runReader (runExceptT ti)

checkTyCtx :: TyCtx -> TI ()
checkTyCtx [] = pure ()
checkTyCtx ((_,ctx):tyCtx) = checkCtx ctx >> checkTyCtx tyCtx

checkCtx :: Ctx -> TI ()
checkCtx [] = pure ()
checkCtx (typ:ctx) = checkType typ []

checkType :: Expr -> Kind -> TI ()
checkType e k = inferType e >>= zipWithM_ betaeq k

inferType :: Expr -> TI Kind
inferType UnitType = pure []
inferType (TypeVar var) = do
  ty <- view tyCtx >>= lookupTI var
  view ctx >>= checkCtx
  pure ty
inferType (a :@: t) = inferType a >>= \case
    [] -> throwError "Can't apply someting to a type with a empty context"
    (b:gamma2) -> do
      (ctx,b') <- inferTerm t
      assert (null ctx) "Type Ctx should be empty"
      betaeq b b'
      pure $ substCtx 0 t gamma2
inferType (Abstr tyX b) = (tyX:) <$> local (ctx %~ (tyX:)) (inferType b)
inferType _        = throwError "This is not a type"

checkTerm :: Ctx -> Expr -> (Ctx,Expr) -> TI ()
checkTerm = undefined

inferTerm :: Expr -> TI Type
inferTerm UnitExpr = pure ([],UnitType)
inferTerm (LocalExprVar idx) = ([],) <$> (view ctx >>= lookupLocalVarTI idx)
inferTerm (GlobalExprVar x) = view defCtx >>= lookupDefTypeTI x
inferTerm (t :@: s) = inferTerm t >>= \case
  ([],_) -> throwError "Can't apply someting to a type with a empty context"
  (a:ctx2,b) -> do
    (ctx,a') <- inferTerm s
    assert (null ctx) "Type Ctx should be empty"
    betaeq a a'
    pure (substCtx 0 s ctx2, substExpr 0 s b)
inferTerm (Constructor x) = view strCtx >>= lookupTI x
inferTerm (Destructor x) = view strCtx >>= lookupTI x
inferTerm Rec{..} = undefined
inferTerm _  = throwError "This is not a term"

betaeq :: Expr -> Expr -> TI ()
betaeq e1 e2 = do
  ee1 <- eval e1
  ee2 <- eval e2
  if ee1 == ee2
  then pure ()
  else throwError $ "couldn't match type "
                  <> T.pack (show e1)
                  <> " with type "
                  <> T.pack (show e2)

eval :: Expr -> TI Expr
eval (GlobalExprVar x) = undefined
eval x = pure x

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j)
  | i == j = r
  | otherwise = v
substExpr i r (e1 :@: e2) = substExpr i r e1 :@: substExpr i r e2
substExpr i r1 (Abstr t r2) = Abstr (substExpr i r1 t) (substExpr (i+1) r1 r2)
substExpr _ _ e = e

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) = substExpr i r e : substCtx (i+1) r ctx

substExprs :: Int -> [Expr] -> Expr -> Expr
substExprs _ [] e = e
substExprs n (v:vs) e = substExprs (n+1) vs (substExpr n v e)

substData :: Text -> Expr -> Expr -> Expr
substData n x (Inductive m)
  | n == m = x
substData n x (Coinductive m)
  | n == m = x
substData n x (e1 :@: e2) = substData n x e1 :@: substData n x e2
substData n x (Abstr t e) = Abstr (substData n x t) (substData n x e)
substData n x Rec{..} = Rec { from = substData n x from
                            , to = substData n x to
                            , matches = map (\m -> m {matchExpr = substData n x (matchExpr m)}) matches
                            }
substData n x atom    = atom

lookupTI :: Eq a => a -> [(a,b)] -> TI b
lookupTI var ctx = case lookup var ctx of
                     Just t   -> pure t
                     Nothing -> throwError "Variable not defined"

lookupLocalVarTI :: Int -> [a] -> TI a
lookupLocalVarTI _ []     = throwError "Variable not defined"
lookupLocalVarTI 0 (x:_)  = pure x
lookupLocalVarTI n (x:xs) = lookupLocalVarTI (n-1) xs

lookupDefTypeTI :: Text -> [Statement] -> TI Type
lookupDefTypeTI _ [] = throwError "Variable not defined"
lookupDefTypeTI n (ExprDef{..}:stmts)
  | n == name = pure $ fromJust ty
  | otherwise = lookupDefTypeTI n stmts
lookupDefTypeTI n (_:stmts) = lookupDefTypeTI n stmts

lookupDefKindTI :: Text -> [Statement] -> TI Kind
lookupDefKindTI _ [] = throwError "Variable not defined"
lookupDefKindTI n (InductiveDef{..}:stmts)
  | n == name = pure gamma
  | otherwise = lookupDefKindTI n stmts
lookupDefKindTI n (CoinductiveDef{..}:stmts)
  | n == name = pure gamma
  | otherwise = lookupDefKindTI n stmts
lookupDefKindTI n (_:stmts) = lookupDefKindTI n stmts

assert :: Bool -> Text -> TI ()
assert True  _   = pure ()
assert False msg = throwError msg

{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow (second)

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

checkTerm :: Expr -> (Ctx,Expr) -> TI ()
checkTerm e (ctx1,a1) = do
  (ctx2, a2) <- inferTerm e
  zipWithM_ betaeq ctx1 ctx2
  betaeq a1 a2

inferTerm :: Expr -> TI Type
inferTerm UnitExpr = pure ([],UnitType)
inferTerm (LocalExprVar idx) = ([],) <$> (view ctx >>= lookupLocalVarTI idx)
inferTerm (GlobalExprVar x) = lookupDefTypeTI x
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
  ee1 <- evalExpr e1
  ee2 <- evalExpr e2
  if ee1 == ee2
  then pure ()
  else throwError $ "couldn't match type "
                  <> T.pack (show e1)
                  <> " with type "
                  <> T.pack (show e2)

evalExpr :: Expr -> TI Expr
evalExpr (Abstr ty expr) = Abstr <$> evalExpr ty <*> evalExpr expr
evalExpr r@Rec{..} = Rec fromRec
                         <$> evalExpr toRec
                         <*> mapM (\Match{..} -> Match structorName
                                              <$> evalExpr matchExpr)
                                  matches
evalExpr r@Corec{..} = Corec <$> evalExpr fromCorec
                             <*> pure toCorec
                             <*> mapM (\Match{..} -> Match structorName
                                                  <$> evalExpr matchExpr)
                                      matches
evalExpr (f :@: arg) = do
  valF <- evalExpr f
  valF' <- case valF of
            GlobalExprVar x -> lookupDefExprTI x
            _               -> pure valF
  valArg <- evalExpr arg
  case (valF', valArg) of
    (Abstr _ expr,_) -> evalExpr $ substExpr 0 expr valArg
    -- TODO gamma can be empty
    (r@Rec{..}, getArgs -> (Constructor constrN,constrArgs)) -> do
      typeTo <- inferType toRec
      recTy <- inferTerm r
      evalExpr $ applyToStr fromRec (getRecRHS constrN matches)
               $ substExprs 0 constrArgs r
    (d@(Destructor n), _) -> undefined
    _ -> pure $ valF :@: valArg
evalExpr atom = pure atom

getRecRHS :: Text -> [Match] -> Expr
getRecRHS str [] = error . T.unpack $ "internel type checking error: rec expression doesn't contain structor" <> str
getRecRHS str (Match{..}:ms)
  | str == structorName = matchExpr
  | otherwise           = getRecRHS str ms

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
substData n x Rec{..} = Rec { fromRec = fromRec
                            , toRec = substData n x toRec
                            , matches = map (\m -> m {matchExpr = substData n x (matchExpr m)}) matches
                            }
substData n x Corec{..} = Corec { fromCorec = substData n x fromCorec
                                , toCorec = toCorec
                                , matches = map (\m -> m {matchExpr = substData n x (matchExpr m)}) matches
                                }
substData n x atom    = atom

applyToStr :: Text -> Expr -> Expr -> Expr
applyToStr n e (getArgs -> (c@(Constructor m),args))
  | n == m = e :@: applyArgs (c,map (applyToStr n e) args)
  | otherwise = applyArgs (c,map (applyToStr n e) args)
applyToStr n e (getArgs -> (d@(Destructor m),args))
  | n == m = e :@: applyArgs (d,map (applyToStr n e) args)
  | otherwise = applyArgs (d,map (applyToStr n e) args)
applyToStr n e (f :@: arg) = applyToStr n e f :@: applyToStr n e arg
applyToStr n e (Abstr ty arg) = Abstr (applyToStr n e ty)
                                      (applyToStr n e arg)
applyToStr n e Rec{..} = Rec { fromRec = fromRec
                             , toRec = applyToStr n e toRec
                             , matches = map (\m -> m {matchExpr = applyToStr n e (matchExpr m)}) matches
                             }
applyToStr n e Corec{..} = Corec { fromCorec = applyToStr n e fromCorec
                                 , toCorec = toCorec
                                 , matches = map (\m -> m {matchExpr = applyToStr n e (matchExpr m)}) matches
                                 }
applyToStr _ _ atom = atom

-- | splits up a chain of left associative applications into a list of
-- arguments
getArgs :: Expr -> (Expr,[Expr])
getArgs (expr :@: arg) = second (++ [arg]) (getArgs expr)
getArgs arg = (arg,[])

applyArgs :: (Expr,[Expr]) -> Expr
applyArgs (f,args) = foldl (:@:) f args

-- | lookup lifted to the TI monad
lookupTI :: Eq a => a -> [(a,b)] -> TI b
lookupTI var ctx = case lookup var ctx of
                     Just t   -> pure t
                     Nothing -> throwError "Variable not defined"

lookupLocalVarTI :: Int -> [a] -> TI a
lookupLocalVarTI _ []     = throwError "Variable not defined"
lookupLocalVarTI 0 (x:_)  = pure x
lookupLocalVarTI n (x:xs) = lookupLocalVarTI (n-1) xs

lookupDefTypeTI :: Text -> TI Type
lookupDefTypeTI t = view defCtx >>= lookupDefTypeTI' t
  where
    lookupDefTypeTI' _ [] = throwError "Variable not defined"
    lookupDefTypeTI' n (ExprDef{..}:stmts)
      | n == name = pure $ fromJust ty
      | otherwise = lookupDefTypeTI' n stmts
    lookupDefTypeTI' n (_:stmts) = lookupDefTypeTI' n stmts

lookupDefExprTI :: Text -> TI Expr
lookupDefExprTI t = view defCtx >>= lookupDefExprTI' t
  where
    lookupDefExprTI' _ [] = throwError "Variable not defined"
    lookupDefExprTI' n (ExprDef{..}:stmts)
      | n == name = pure expr
      | otherwise = lookupDefExprTI' n stmts
    lookupDefExprTI' n (_:stmts) = lookupDefExprTI' n stmts

lookupDefKindTI :: Text -> TI Kind
lookupDefKindTI t = view defCtx >>= lookupDefKindTI' t
  where
    lookupDefKindTI' _ [] = throwError "Variable not defined"
    lookupDefKindTI' n (InductiveDef{..}:stmts)
      | n == name = pure gamma
      | otherwise = lookupDefKindTI' n stmts
    lookupDefKindTI' n (CoinductiveDef{..}:stmts)
      | n == name = pure gamma
      | otherwise = lookupDefKindTI' n stmts
    lookupDefKindTI' n (_:stmts) = lookupDefKindTI' n stmts

assert :: Bool -> Text -> TI ()
assert True  _   = pure ()
assert False msg = throwError msg

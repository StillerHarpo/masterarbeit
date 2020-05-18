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
checkTyCtx = mapM_ checkCtx

checkCtx :: Ctx -> TI ()
checkCtx [] = pure ()
checkCtx (typ:ctx') =
  local (over tyCtx (const []) . over ctx (const ctx'))
        (checkType typ [])
  >> checkCtx ctx'

checkType :: TypeExpr -> Kind -> TI ()
checkType e k = inferType e >>= zipWithM_ betaeq k

inferType :: TypeExpr -> TI Kind
inferType UnitType = pure []
inferType (LocalTypeVar idx) = do
  ty <- view tyCtx >>= lookupLocalVarTI idx
  view ctx >>= checkCtx
  pure ty
inferType (GlobalTypeVar var) = lookupDefKindTI var
inferType (a :@ t) = inferType a >>= \case
    [] -> throwError "Can't apply someting to a type with a empty context"
    (b:gamma2) -> do
      (ctx,b') <- inferTerm t
      assert (null ctx) "Type Ctx should be empty"
      betaeq b b'
      pure $ substCtx 0 t gamma2
inferType (Abstr tyX b) = (tyX:) <$> local (ctx %~ (tyX:)) (inferType b)
inferType (In d) = inferTypeDuctive d
inferType (Coin d) = inferTypeDuctive d

inferTypeDuctive :: Ductive -> TI Kind
inferTypeDuctive Ductive{..} = do
  -- TODO check context morphism
  zipWithM_ (\gamma1 a ->
              local (over tyCtx (gamma:) . over ctx (gamma1++)) (checkType a []))
            gamma1s as
  pure gamma

checkTerm :: Expr -> Type -> TI ()
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
    pure (substCtx 0 s ctx2, substTypeExpr 0 s b)
inferTerm (Constructor d@Ductive{..} i) =
  inferTypeDuctive d
  >> pure ( substType 0 (as !! i) (In d) : gamma1s !! i
          , applyTypeExprArgs (In d, sigmas !! i))
inferTerm (Destructor d@Ductive{..} i) =
  inferTypeDuctive d
  >> pure ( substType 0 (as !! i) (Coin d) : gamma1s !! i
          , applyTypeExprArgs (Coin d, sigmas !! i))
inferTerm Rec{..} = do
  valTo <- evalTypeExpr toRec
  valFrom <- evalDuctive fromRec
  gamma' <- inferType valTo
  let Ductive{..} = valFrom
  zipWithM_ betaeq gamma' gamma
  sequence_ $ zipWith4 (\ gamma1 sigma a match ->
                          local (over ctx ((substType 0 valTo a:gamma1)++))
                                (checkTerm match ([],applyTypeExprArgs (valTo,sigma))))
                       gamma1s sigmas as matches
  pure ( applyTypeExprArgs (In valFrom, idCtx gamma):gamma
       , applyTypeExprArgs (valTo,idCtx gamma))
inferTerm Corec{..} = do
  valTo <- evalDuctive toCorec
  valFrom <- evalTypeExpr fromCorec
  gamma' <- inferType valFrom
  let Ductive{..} = valTo
  zipWithM_ betaeq gamma' gamma
  sequence_ $ zipWith4 (\ gamma1 sigma a match ->
                          local (over ctx ((applyTypeExprArgs (valFrom,sigma):gamma1)++))
                                (checkTerm match ([],substType 0 valFrom a)))
                       gamma1s sigmas as matches
  pure ( applyTypeExprArgs (valFrom, idCtx gamma):gamma
       , applyTypeExprArgs (Coin valTo,idCtx gamma))

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _      _      _      _      = []

betaeq :: TypeExpr -> TypeExpr -> TI ()
betaeq e1 e2 = do
  ee1 <- evalTypeExpr e1
  ee2 <- evalTypeExpr e2
  if ee1 == ee2
  then pure ()
  else throwError $ "couldn't match type "
                  <> T.pack (show e1)
                  <> " with type "
                  <> T.pack (show e2)

evalTypeExpr :: TypeExpr -> TI TypeExpr
evalTypeExpr (Abstr ty expr) = Abstr <$> evalTypeExpr ty
                                     <*> evalTypeExpr expr
evalTypeExpr (f :@ arg) = do
  valF <- evalTypeExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    (Abstr _ expr,_) -> evalTypeExpr $ substTypeExpr 0 valArg expr
    _ -> pure $ valF :@ valArg
evalTypeExpr (In d) = In <$> evalDuctive d
evalTypeExpr (Coin d) = Coin <$> evalDuctive d
evalTypeExpr atom = pure atom

evalCtx :: Ctx -> TI Ctx
evalCtx = mapM evalTypeExpr

evalDuctive :: Ductive -> TI Ductive
evalDuctive Ductive{..} = do
  gamma <- evalCtx gamma
  sigmas <- mapM (mapM evalExpr) sigmas
  as <- local (over tyCtx (gamma:)) $ mapM evalTypeExpr as
  gamma1s <- mapM evalCtx gamma1s
  pure Ductive{..}

evalExpr :: Expr -> TI Expr
evalExpr r@Rec{..} = Rec fromRec
                         <$> evalTypeExpr toRec
                         <*> mapM evalExpr matches
evalExpr r@Corec{..} = Corec <$> evalTypeExpr fromCorec
                             <*> pure toCorec
                             <*> mapM evalExpr matches
evalExpr (f :@: arg) = do
  valF <- evalExpr f
  valF' <- case valF of
            GlobalExprVar x -> lookupDefExprTI x
            _               -> pure valF
  valArg <- evalExpr arg
  case (valF', valArg) of
    -- TODO gamma can be empty
    (r@Rec{..}, getExprArgs -> (Constructor ductive i,constrArgs)) -> do
      typeTo <- inferType toRec
      recTy <- inferTerm r
      undefined
    (d@(Destructor ductive i), _) -> undefined
    _ -> pure $ valF :@: valArg
evalExpr atom = pure atom

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j)
  | i == j = r
  | otherwise = v
substExpr i r (GlobalExprVar var) = undefined
substExpr i r (e1 :@: e2) = substExpr i r e1 :@: substExpr i r e2
substExpr _ _ e = e

substTypeExpr :: Int -> Expr -> TypeExpr -> TypeExpr
substTypeExpr i r1 (Abstr t r2) = Abstr (substTypeExpr i r1 t) (substTypeExpr (i+1) r1 r2)
substTypeExpr i r1 (In d) = In $ substDuctiveExpr i r1 d
substTypeExpr i r1 (Coin d) = Coin $ substDuctiveExpr i r1 d
substTypeExpr i r (e1 :@ e2) = substTypeExpr i r e1 :@ substExpr i r e2
substTypeExpr _ _ e = e

substDuctiveExpr :: Int -> Expr -> Ductive -> Ductive
substDuctiveExpr i r1 Ductive{..} = Ductive { gamma = substCtx i r1 gamma
                                            , sigmas = map (map $ substExpr i r1) sigmas
                                            , as = map (substTypeExpr i r1) as
                                            , gamma1s = map (substCtx i r1) gamma1s
                                            }

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) = substTypeExpr i r e : substCtx (i+1) r ctx

substExprs :: Int -> [Expr] -> Expr -> Expr
substExprs _ [] e = e
substExprs n (v:vs) e = substExprs (n+1) vs (substExpr n v e)

substType :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substType i r v@(LocalTypeVar j)
  | i == j = r
  | otherwise = v
substType i r (GlobalTypeVar var) = undefined
substType i r1 (Abstr t r2) = Abstr (substType i r1 t) (substType i r1 r2)
substType i r (In d) = In $ substDuctiveTypeExpr i r d
substType i r (Coin d) = Coin $ substDuctiveTypeExpr i r d
substType i r (e1 :@ e2) = substType i r e1 :@ e2
substType _ _ e = e

substDuctiveTypeExpr :: Int -> TypeExpr -> Ductive -> Ductive
substDuctiveTypeExpr i r1 d@Ductive{..} = d { as = map (substType (i+1) r1) as }


-- | splits up a chain of left associative applications to a expression
-- into a list of  arguments
getExprArgs :: Expr -> (Expr,[Expr])
getExprArgs (expr :@: arg) = second (++ [arg]) (getExprArgs expr)
getExprArgs arg = (arg,[])

applyExprArgs :: (Expr,[Expr]) -> Expr
applyExprArgs (f,args) = foldl (:@:) f args

idCtx :: Ctx -> [Expr]
idCtx ctx = LocalExprVar <$> take (length ctx) [0..]

-- | splits up a chain of left associative applications to a type into a
-- list of  arguments
getTypeExprArgs :: TypeExpr -> (TypeExpr,[Expr])
getTypeExprArgs (expr :@ arg) = second (++ [arg]) (getTypeExprArgs expr)
getTypeExprArgs arg = (arg,[])

applyTypeExprArgs :: (TypeExpr,[Expr]) -> TypeExpr
applyTypeExprArgs (f,args) = foldl (:@) f args

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
    lookupDefKindTI' n (TypeDef{..}:stmts)
      | n == name = pure $ fromJust kind
      | otherwise = lookupDefKindTI' n stmts
    lookupDefKindTI' n (_:stmts) = lookupDefKindTI' n stmts

lookupDefTypeExprTI :: Text -> TI TypeExpr
lookupDefTypeExprTI t = view defCtx >>= lookupDefTypeExprTI' t
  where
    lookupDefTypeExprTI' _ [] = throwError "Variable not defined"
    lookupDefTypeExprTI' n (TypeDef{..}:stmts)
      | n == name = pure typeExpr
      | otherwise = lookupDefTypeExprTI' n stmts
    lookupDefTypeExprTI' n (_:stmts) = lookupDefTypeExprTI' n stmts

assert :: Bool -> Text -> TI ()
assert True  _   = pure ()
assert False msg = throwError msg

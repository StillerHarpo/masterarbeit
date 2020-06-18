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

import Data.List

import Lens.Micro.Platform

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (fromJust)

import AbstractSyntaxTree

import PrettyPrinter

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
inferType v@(LocalTypeVar idx _) = do
  ty <- view tyCtx >>= lookupLocalVarTI idx (T.pack $ show v)
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
  -- in sigmas shouldn't be variables wich refer to unittype
  zipWithM_ (\sigma gamma1 -> checkContextMorph sigma (gamma1++[UnitType]) gamma) sigmas gamma1s
  zipWithM_ (\gamma1 a ->
              local (over tyCtx (gamma:) . set ctx gamma1) (checkType a []))
            gamma1s as
  pure gamma

checkContextMorph :: [Expr] -> Ctx -> Ctx -> TI ()
checkContextMorph [] gamma1 [] = checkCtx gamma1
checkContextMorph [] _ _ = throwError $ "Invalid context morphism:"
                                        <> "Gamma2 should be empty for empty morphism"
checkContextMorph (t:ts) gamma1 (a:gamma2) = do
  local (over ctx (const gamma1)) (checkTerm t  ([], substTypeExprs 0 ts a))
  checkContextMorph ts gamma1 gamma2

checkTerm :: Expr -> Type -> TI ()
checkTerm e (ctx1,a1) = do
  (ctx2, a2) <- inferTerm e
  zipWithM_ betaeq ctx1 ctx2
  betaeq a1 a2

inferTerm :: Expr -> TI Type
inferTerm UnitExpr = pure ([],UnitType)
inferTerm v@(LocalExprVar idx _) =
  ([],) <$> (view ctx >>= lookupLocalVarTI idx(T.pack $ show v))
inferTerm (GlobalExprVar x) = lookupDefTypeTI x
inferTerm (t :@: s) = inferTerm t >>= \case
  ([],_) -> throwError "Can't apply something to a term with a empty context"
  (a:ctx2,b) -> do
    (ctx,a') <- inferTerm s
    assert (null ctx) "Type Ctx should be empty"
    betaeq a a'
    pure (substCtx 0 s ctx2, substTypeExpr 0 s b)
inferTerm (Constructor d@Ductive{..} i _) =
  inferTypeDuctive d
  >> pure (gamma1s !! i ++ [substType 0 (In d) (as !! i)]
          , applyTypeExprArgs (In d, sigmas !! i))
inferTerm (Destructor d@Ductive{..} i _) =
  inferTypeDuctive d
  >> pure (gamma1s !! i ++ [substType 0 (Coin d) (as !! i)]
          , applyTypeExprArgs (Coin d, sigmas !! i))
inferTerm Rec{..} = do
  valTo <- evalTypeExpr toRec
  valFrom <- evalDuctive fromRec
  gamma' <- inferType valTo
  let Ductive{..} = valFrom
  zipWithM_ betaeq gamma' gamma
  sequence_ $ zipWith4 (\ gamma1 sigma a match ->
                          local (over ctx (++gamma1++[substType 0 valTo a]))
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
                          local (over ctx (++gamma1++[applyTypeExprArgs (valFrom,sigma)]))
                                (checkTerm match ([],substType 0 valFrom a)))
                       gamma1s sigmas as matches
  pure ( applyTypeExprArgs (valFrom, idCtx gamma):gamma
       , applyTypeExprArgs (Coin valTo,idCtx gamma))

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
    -- TODO Should we check if _ = sigma_k\cric \tau
    ( getExprArgs -> (r@Rec{..}, _), getExprArgs -> (Constructor _ i _, constrArgs)) -> do
      (_,typeU) <- inferTerm (last constrArgs) -- has to be A_k[\mu/X]
      evalExpr $ substExprs 0 constrArgs
                         (substExpr 0 (typeAction 0 (as fromRec !! i)
                                                    [applyExprArgs (r, idCtx (gamma fromRec))
                                                     :@: LocalExprVar 0 Nothing]
                                                    [gamma1s fromRec !! i]
                                                    [typeU]
                                                    [toRec])
                                      (matches !! i))
    (getExprArgs -> (Destructor ductive i _, tau) , getExprArgs -> (c@Corec{..}, args)) -> do
      (_,typeU) <- inferTerm (matches !! i)
      evalExpr $ substExprs 0 (tau ++ [last args])
                          (substExpr 0 (matches !! i)
                                       (typeAction 0 (as toCorec !! i)
                                                     [applyExprArgs (c, idCtx (gamma toCorec))
                                                      :@: LocalExprVar 0 Nothing]
                                                     [gamma1s toCorec !! i]
                                                     [typeU]
                                                     [fromCorec]))
    _ -> pure $ valF :@: valArg
evalExpr atom = pure atom

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j _)
  | i == j = r
  | otherwise = v
substExpr i r (GlobalExprVar var) = undefined
substExpr i r (e1 :@: e2) = substExpr i r e1 :@: substExpr i r e2
substExpr i r re@Rec{..} = let Ductive{..} = fromRec
                               newMatches  = zipWith (\i m -> substExpr i r m)
                                                     (map ((+1) . (+i) . length) gamma1s)
                                                     matches
                           in re { matches = newMatches}
substExpr i r c@Corec{..} = let Ductive{..} = toCorec
                                newMatches  = zipWith (\i m -> substExpr i r m)
                                                      (map ((+1) . (+i) . length) gamma1s)
                                                      matches
                            in c { matches = newMatches }
substExpr _ _ e = e

substTypeExpr :: Int -> Expr -> TypeExpr -> TypeExpr
substTypeExpr i r1 (Abstr t r2) = Abstr (substTypeExpr i r1 t) (substTypeExpr (i+1) r1 r2)
substTypeExpr i r1 (In d) = In $ substDuctiveExpr i r1 d
substTypeExpr i r1 (Coin d) = Coin $ substDuctiveExpr i r1 d
substTypeExpr i r (e1 :@ e2) = substTypeExpr i r e1 :@ substExpr i r e2
substTypeExpr _ _ e = e

substDuctiveExpr :: Int -> Expr -> Ductive -> Ductive
substDuctiveExpr i r1 dIn@Ductive{..} =
  let dOut = Ductive { gamma = substCtx i r1 gamma
                     , sigmas = zipWith (\s g -> map (substExpr (i+length g) r1) s) sigmas gamma1s
                     , as = zipWith (`substTypeExpr` r1) (map ((+i) . length) gamma1s) as
                     , gamma1s = map (substCtx i r1) gamma1s
                     , nameDuc = Nothing
                     }
  in if dOut == dIn then dIn else dOut

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) = substTypeExpr i r e : substCtx (i+1) r ctx

substExprs :: Int -> [Expr] -> Expr -> Expr
substExprs _ [] e = e
substExprs n (v:vs) e = substExprs (n+1) vs (substExpr n v e)

substTypeExprs :: Int -> [Expr] -> TypeExpr -> TypeExpr
substTypeExprs _ [] e = e
substTypeExprs n (v:vs) e = substTypeExprs (n+1) vs (substTypeExpr n v e)

substType :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substType i r v@(LocalTypeVar j _)
  | i == j = r
  | otherwise = v
substType i r (GlobalTypeVar var) = undefined
substType i r1 (Abstr t r2) = Abstr (substType i r1 t) (substType i r1 r2)
substType i r (In d) = In $ substDuctiveTypeExpr i r d
substType i r (Coin d) = Coin $ substDuctiveTypeExpr i r d
substType i r (e1 :@ e2) = substType i r e1 :@ e2
substType _ _ e = e

substTypes :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substTypes _ [] e = e
substTypes n (v:vs) e = substTypes (n+1) vs (substType n v e)

substDuctiveTypeExpr :: Int -> TypeExpr -> Ductive -> Ductive
substDuctiveTypeExpr i r1 dOld@Ductive{..} =
  let dNew =  dOld { as = map (substType (i+1) r1) as
                   , nameDuc = Nothing }
  in if dNew == dOld then dOld else dNew


typeAction :: Int -> TypeExpr -> [Expr] -> [Ctx] -> [TypeExpr] -> [TypeExpr] -> Expr
typeAction i (LocalTypeVar n _) terms _ _ _ = terms !! n
typeAction i (GlobalTypeVar n) terms _ _ _ = undefined
typeAction i (c :@ s) terms gammas as' bs = substExpr 0 (typeAction i c terms gammas as' bs) s
typeAction i (Abstr _ c) terms gammas as' bs = typeAction i c terms gammas as' bs
typeAction i (In d) terms gammas as' bs =
  let idDeltas = map idCtx (gamma1s d)
      fromRec = d { as = map (substTypes 0 (zipWith abstrArgs as' gammas)) (as d)
                  , nameDuc = Nothing} -- R_a in paper
      toRecDuctive = d { as = map (substTypes 0 (zipWith abstrArgs bs gammas)) (as d)
                       , nameDuc = Nothing}
      toRec = In toRecDuctive
      matches = zipWith5 (\idDelta dk a b i ->
                             let j = 1 + length idDelta
                             in applyExprArgs (Constructor toRecDuctive i Nothing, init idDelta)
                                :@: typeAction i dk
                                               -- TODO is this the right variable?
                                               -- Could be bound by wrong binder.
                                               (LocalExprVar j Nothing : terms)
                                               (gamma d:gammas)
                                               (a:as')
                                               (b:bs)) -- g_k in paper
                              idDeltas (as d) (as fromRec) (as toRecDuctive) [1..]
  in applyExprArgs (Rec {..}  ,idCtx (gamma d)) :@: LocalExprVar i Nothing
typeAction i (Coin d) terms gammas as' bs =
  let idDeltas = map idCtx (gamma1s d)
      fromCorecDuctive = d { as = map (substTypes 0 (zipWith abstrArgs as' gammas)) (as d)
                           , nameDuc = Nothing} -- R_a in paper
      fromCorec = Coin fromCorecDuctive
      toCorec = d { as = map (substTypes 0 (zipWith abstrArgs bs gammas)) (as d) }
      matches = zipWith5 (\idDelta dk a b i ->
                             let j = 1 + length idDelta
                             in substExpr 0 (typeAction i dk
                                               -- TODO is this the right variable?
                                               -- Could be bound by wrong binder.
                                                       (LocalExprVar j Nothing : terms)
                                                       (gamma d:gammas)
                                                       (a:as')
                                                       (b:bs))
                                            (applyExprArgs (Destructor fromCorecDuctive i Nothing
                                                           , idDelta))) -- g_k in paper
                              idDeltas (as d) (as fromCorecDuctive) (as toCorec) [1..]
  in applyExprArgs (Corec {..}  ,idCtx (gamma d)) :@: LocalExprVar i Nothing
typeAction i _ _ _ _ _ = LocalExprVar i Nothing

-- | splits up a chain of left associative applications to a expression
-- into a list of  arguments
getExprArgs :: Expr -> (Expr,[Expr])
getExprArgs (expr :@: arg) = second (++ [arg]) (getExprArgs expr)
getExprArgs arg = (arg,[])

applyExprArgs :: (Expr,[Expr]) -> Expr
applyExprArgs (f,args) = foldl (:@:) f args

abstrArgs :: TypeExpr -> [TypeExpr] -> TypeExpr
abstrArgs = foldr Abstr

idCtx :: Ctx -> [Expr]
idCtx ctx = take (length ctx) $ map (flip LocalExprVar Nothing) [0..]

-- | splits up a chain of left associative applications to a type into a
-- list of  arguments
getTypeExprArgs :: TypeExpr -> (TypeExpr,[Expr])
getTypeExprArgs (expr :@ arg) = second (++ [arg]) (getTypeExprArgs expr)
getTypeExprArgs arg = (arg,[])

applyTypeExprArgs :: (TypeExpr,[Expr]) -> TypeExpr
applyTypeExprArgs (f,args) = foldl (:@) f args

lookupLocalVarTI :: Int -> Text -> [a] -> TI a
lookupLocalVarTI i t ctx = lookupLocalVarTI' i (reverse ctx)
  where
    lookupLocalVarTI' :: Int -> [a] -> TI a
    lookupLocalVarTI' _ []     = throwError (t <> " not defined")
    lookupLocalVarTI' 0 (x:_)  = pure x
    lookupLocalVarTI' n (x:xs)
      | n < 0 = error "internal error negative variable lookup"
      | otherwise = lookupLocalVarTI' (n-1) xs

lookupDefTypeTI :: Text -> TI Type
lookupDefTypeTI t = view defCtx >>= lookupDefTypeTI'
  where
    lookupDefTypeTI' :: [Statement] -> TI Type
    lookupDefTypeTI' [] = throwError $ "Variable " <> t <> " not defined"
    lookupDefTypeTI' (ExprDef{..}:stmts)
      | t == name = pure $ fromJust ty
      | otherwise = lookupDefTypeTI' stmts
    lookupDefTypeTI' (_:stmts) = lookupDefTypeTI' stmts

lookupDefExprTI :: Text -> TI Expr
lookupDefExprTI t = view defCtx >>= lookupDefExprTI'
  where
    lookupDefExprTI' :: [Statement] -> TI Expr
    lookupDefExprTI' [] = throwError $ "Variable " <> t <> " not defined"
    lookupDefExprTI' (ExprDef{..}:stmts)
      | t == name = pure expr
      | otherwise = lookupDefExprTI' stmts
    lookupDefExprTI' (_:stmts) = lookupDefExprTI' stmts

lookupDefKindTI :: Text -> TI Kind
lookupDefKindTI t = view defCtx >>= lookupDefKindTI'
  where
    lookupDefKindTI' :: [Statement] -> TI Kind
    lookupDefKindTI' [] = throwError $ "Variable " <> t <> " not defined"
    lookupDefKindTI' (TypeDef{..}:stmts)
      | t == name = pure $ fromJust kind
      | otherwise = lookupDefKindTI' stmts
    lookupDefKindTI' (_:stmts) = lookupDefKindTI' stmts

lookupDefTypeExprTI :: Text -> TI TypeExpr
lookupDefTypeExprTI t = view defCtx >>= lookupDefTypeExprTI'
  where
    lookupDefTypeExprTI' :: [Statement] -> TI TypeExpr
    lookupDefTypeExprTI' [] = throwError $ "Variable " <> t <> " not defined"
    lookupDefTypeExprTI' (TypeDef{..}:stmts)
      | t == name = pure typeExpr
      | otherwise = lookupDefTypeExprTI' stmts
    lookupDefTypeExprTI' (_:stmts) = lookupDefTypeExprTI' stmts

assert :: Bool -> Text -> TI ()
assert True  _   = pure ()
assert False msg = throwError msg

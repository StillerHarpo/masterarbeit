{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TemplateHaskell #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}

module TypeChecker where

import Debug.Trace

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Arrow (second)

import Data.List

import Lens.Micro.Platform

import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (fromJust)

import AbstractSyntaxTree
import Data.Text.Prettyprint.Doc

import PrettyPrinter

data ContextTI = ContextTI { _ctx :: Ctx
                           , _tyCtx :: TyCtx
                           , _parCtx :: TyCtx
                           , _defCtx :: [Statement]
                           }
$(makeLenses ''ContextTI)

type TI ann = ExceptT (Doc ann) (Reader ContextTI)

type PTI ann = ExceptT (Doc ann) (State [Statement])

runTI :: TI ann a -> ContextTI -> Either (Doc ann) a
runTI ti = runReader (runExceptT ti)

evalPTI :: PTI ann a -> [Statement] -> Either (Doc ann) a
evalPTI pti = evalState (runExceptT pti)

emptyCtx :: ContextTI
emptyCtx = ContextTI { _ctx = []
                     , _tyCtx = []
                     , _parCtx = []
                     , _defCtx = []}

checkProgram :: [Statement] -> Either (Doc ann) [TypedExpr]
checkProgram = flip evalPTI [] . checkProgramPTI

checkProgramPTI :: [Statement] -> PTI ann [TypedExpr]
checkProgramPTI [] = pure []
checkProgramPTI (ExprDef{..} : stmts) = do
  tiInPTI $ checkParCtx tyParameterCtx
  tiInPTI $ local (set parCtx tyParameterCtx) $ checkCtx exprParameterCtx
  ty <- Just <$> tiInPTI (local (set parCtx tyParameterCtx
                                 . set ctx exprParameterCtx)
                                (inferTerm expr >>= evalType))
  expr <- tiInPTI $ evalExpr expr
  modify (ExprDef{..} :)
  checkProgramPTI stmts
checkProgramPTI (TypeDef{..} : stmts) = do
  tiInPTI $ checkParCtx parameterCtx
  kind <- Just <$> tiInPTI (local (set parCtx parameterCtx)
                                  (inferType typeExpr >>= evalCtx))
  typeExpr <- tiInPTI $ evalTypeExpr typeExpr
  modify (TypeDef{..} :)
  checkProgramPTI stmts
checkProgramPTI (Expression expr : stmts) =
  (:) <$> (TypedExpr <$> tiInPTI (evalExpr expr)
                     <*> tiInPTI (inferTerm expr >>= evalType))
      <*> checkProgramPTI stmts
tiInPTI :: TI ann a -> PTI ann a
tiInPTI ti = do
  curDefCtx <- get
  case runTI ti $ set defCtx curDefCtx emptyCtx of
    Left err -> throwError err
    Right a -> pure a

checkTyCtx :: TyCtx -> TI ann ()
checkTyCtx = mapM_ checkCtx

checkParCtx :: TyCtx -> TI ann ()
checkParCtx parCtx' =
  catchError (checkParCtx' parCtx')
             (throwError . (<> "\n in parameter context"
                           <+> pretty parCtx'))
    where
      checkParCtx' [] = pure ()
      checkParCtx' (ctx':parCtx') =
        checkCtx ctx'
        >> local (over parCtx (ctx':)) (checkParCtx parCtx')

checkCtx :: Ctx -> TI ann ()
checkCtx ctx' = catchError (checkCtx' ctx')
                           (throwError . (<> "\n in context"
                                          <+> pretty ctx'))
  where
    checkCtx' [] = pure ()
    checkCtx' (typ:ctx') =
      local (over tyCtx (const []) . over ctx (const ctx'))
            (checkType typ [])
      >> checkCtx ctx'

checkType :: TypeExpr -> Kind -> TI ann ()
checkType e k = inferType e >>= betaeqCtx k

inferType :: TypeExpr -> TI ann Kind
inferType tyExpr = catchError (inferType' tyExpr)
                              (throwError . (<> "\n in type expression "
                                             <+> pretty tyExpr))
  where
    inferType' UnitType = pure []
    inferType' v@(LocalTypeVar idx _) = do
      ty <- view tyCtx >>= lookupLocalVarTI idx (T.pack $ show v)
      view ctx >>= checkCtx
      pure ty
    inferType' v@(Parameter idx _) = do
      ty <- view parCtx >>= lookupLocalVarTI idx (T.pack $ show v)
      view ctx >>= checkCtx
      pure (shiftFreeParsKind (idx + 1) 0 ty)
    inferType' (GlobalTypeVar n pars) = lookupDefKindTI n pars
    inferType' (a :@ t) = inferType a >>= \case
        [] -> throwError "Can't apply someting to a type with a empty context"
        (b:gamma2) -> do
          (ctx,b') <- inferTerm t
          assert (null ctx) "Type Ctx should be empty"
          betaeq b b'
          pure $ substCtx 0 t gamma2
    inferType' (Abstr tyX b) = (tyX:) <$> local (ctx %~ (tyX:)) (inferType b)
    inferType' (In d) = local (ctx .~ []) (inferTypeDuctive d)
    inferType' (Coin d) = local (ctx .~ []) (inferTypeDuctive d)

inferTypeDuctive :: Ductive -> TI ann Kind
inferTypeDuctive Ductive{..} = do
  -- in sigmas shouldn't be variables wich refer to unittype
  zipWithM_ (\sigma gamma1 -> checkContextMorph sigma gamma1 gamma) sigmas gamma1s
  zipWithM_ (\gamma1 a ->
              local (over tyCtx (++[gamma]) . set ctx gamma1) (checkType a []))
            gamma1s as
  pure gamma

checkContextMorph :: [Expr] -> Ctx -> Ctx -> TI ann ()
checkContextMorph exprs gamma1 gamma2 =
  catchError (checkContextMorph' exprs gamma1 gamma2)
             (throwError . (<> "\n in context morphismus "
                            <+> pretty exprs))
  where
    checkContextMorph' [] gamma1 [] = checkCtx gamma1
    checkContextMorph' [] _ _ = throwError $ "Invalid context morphism:"
                                            <> "Gamma2 should be empty for empty morphism"
    checkContextMorph' (t:ts) gamma1 (a:gamma2) = do
      local (over ctx (const gamma1)) (checkTerm t ([], substTypeExprs 0 ts a))
      checkContextMorph ts gamma1 gamma2

checkTerm :: Expr -> Type -> TI ann ()
checkTerm e (ctx1,a1) = do
  (ctx2, a2) <- inferTerm e
  betaeqCtx ctx1 ctx2
  betaeq a1 a2

inferTerm :: Expr -> TI ann Type
inferTerm expr = catchError (inferTerm' expr)
                                (throwError . (<> "\n in expression"
                                               <+> pretty expr))
  where
    inferTerm' UnitExpr = pure ([],UnitType)
    inferTerm' v@(LocalExprVar idx _) =
      ([],) . shiftFreeVarsTypeExpr (idx + 1) 0
      <$> (view ctx >>= lookupLocalVarTI idx (T.pack $ show v))
    inferTerm' (GlobalExprVar x tyPars exprPars) = lookupDefTypeTI x tyPars exprPars
    inferTerm' (t :@: s) = inferTerm t >>= \case
      ([],_) -> throwError "Can't apply something to a term with a empty context"
      (a:ctx2,b) -> do
        (ctx,a') <- inferTerm s
        assert (null ctx) "Type Ctx should be empty"
        betaeq a a'
        --shift free vars 0
        pure ( shiftFreeVarsCtx (-1)
                                0
                                (substCtx 0 (shiftFreeVarsExpr 1 0 s) ctx2)
             , shiftFreeVarsTypeExpr
                 (-1)
                     0
                 (substTypeExpr (length ctx2)
                                (shiftFreeVarsExpr (length ctx2+1) 0 s)
                                 b))
    inferTerm' (Constructor d@Ductive{..} i) =
      inferTypeDuctive d
      >> pure (gamma1s !! i ++ [substType 0 (In d) (as !! i)]
              , shiftFreeVarsTypeExpr 1 0 $ applyTypeExprArgs (In d, sigmas !! i))
    inferTerm' (Destructor d@Ductive{..} i) =
      inferTypeDuctive d
      >> pure ( gamma1s !! i ++ [applyTypeExprArgs (Coin d, sigmas !! i)]
              , shiftFreeVarsTypeExpr 1 0 $ substType 0 (Coin d) (as !! i) )
    inferTerm' Rec{..} = do
      valTo <- evalTypeExpr toRec
      valFrom <- evalDuctive fromRec
      gamma' <- inferType valTo
      let Ductive{..} = valFrom
      betaeqCtx gamma' gamma
      sequence_ $ zipWith4 (\ gamma1 sigma a match ->
                              local (over ctx (++gamma1++[substType 0 valTo a]))
                                    (checkTerm match ([],applyTypeExprArgs (valTo,sigma))))
                           gamma1s sigmas as matches
      pure ( gamma ++ [applyTypeExprArgs (In valFrom, idCtx gamma)]
           , applyTypeExprArgs ( valTo
                               , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))
    inferTerm' Corec{..} = do
      valTo <- evalDuctive toCorec
      valFrom <- evalTypeExpr fromCorec
      gamma' <- inferType valFrom
      let Ductive{..} = valTo
      betaeqCtx gamma' gamma
      sequence_ $ zipWith4 (\gamma1 sigma a match ->
                               local (over ctx (++gamma1++[applyTypeExprArgs (valFrom,sigma)]))
                                     (checkTerm match ([], shiftFreeVarsTypeExpr 1 0 $ substType 0 valFrom a)))
                           gamma1s sigmas as matches
      pure ( gamma ++ [applyTypeExprArgs (valFrom, idCtx gamma)]
           , applyTypeExprArgs (Coin valTo
                               , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))
    inferTerm' (WithParameters ps e) = inferTerm $ substParsInExpr 0 (reverse ps) e

betaeq :: TypeExpr -> TypeExpr -> TI ann ()
betaeq e1 e2 = do
  ee1 <- inlineTypeExpr e1 >>= evalTypeExpr
  ee2 <- inlineTypeExpr e2 >>= evalTypeExpr
  if ee1 == ee2
  then pure ()
  else throwError $ "couldn't match type"
                  <+> pretty e1
                  <+> "with type"
                  <+> pretty e2

betaeqCtx :: Ctx -> Ctx -> TI ann ()
betaeqCtx ctx1 ctx2 = do
  assert (length ctx1 == length ctx2)
         ("length of context" <+> pretty ctx1 <>
          "\ndoesn't match length of context" <+> pretty ctx2)
  zipWithM_ betaeq ctx1 ctx2

betaeqTyCtx :: TyCtx -> TyCtx -> TI ann ()
betaeqTyCtx ctx1 ctx2 = do
  assert (length ctx1 == length ctx2)
         ("length of type context " <+> pretty ctx1 <>
          "\ndoesn't match length of type context" <+> pretty ctx2)
  zipWithM_ betaeqCtx ctx1 ctx2

inlineTypeExpr :: TypeExpr -> TI ann TypeExpr
inlineTypeExpr (tyExpr :@ expr) = (:@) <$> inlineTypeExpr tyExpr <*> inlineExpr expr
inlineTypeExpr (GlobalTypeVar n vars) = lookupDefTypeExprTI n vars
                                        >>= inlineTypeExpr
inlineTypeExpr (Abstr tyExpr1 tyExpr2) = Abstr <$> inlineTypeExpr tyExpr1
                                               <*> inlineTypeExpr tyExpr2
inlineTypeExpr (In duc) = In <$> inlineDuctive duc
inlineTypeExpr (Coin duc) = Coin <$> inlineDuctive duc
inlineTypeExpr tyExpr = pure tyExpr

inlineDuctive :: Ductive -> TI ann Ductive
inlineDuctive Ductive{..} = do
  gamma <- inlineCtx gamma
  sigmas <- mapM (mapM inlineExpr) sigmas
  as <- mapM inlineTypeExpr as
  gamma1s <- mapM inlineCtx gamma1s
  pure $ Ductive{..}

inlineExpr :: Expr -> TI ann Expr
inlineExpr (GlobalExprVar n tyPars exprPars) = lookupDefExprTI n tyPars exprPars
  >>= inlineExpr
inlineExpr (e1 :@: e2) = (:@:) <$> inlineExpr e1 <*> inlineExpr e2
inlineExpr Constructor{..} = do
  ductive <- inlineDuctive ductive
  pure Constructor{..}
inlineExpr Destructor{..} = do
  ductive <- inlineDuctive ductive
  pure Destructor{..}
inlineExpr Rec{..} = do
  fromRec <- inlineDuctive fromRec
  toRec <- inlineTypeExpr toRec
  matches <- mapM inlineExpr matches
  pure Rec{..}
inlineExpr Corec{..} = do
  fromCorec <- inlineTypeExpr fromCorec
  toCorec <- inlineDuctive toCorec
  matches <- mapM inlineExpr matches
  pure Corec{..}
inlineExpr (WithParameters tyExprs expr) = WithParameters
                                           <$> mapM inlineTypeExpr tyExprs
                                           <*> inlineExpr expr
inlineExpr expr = pure expr

inlineCtx :: Ctx -> TI ann Ctx
inlineCtx = mapM inlineTypeExpr

evalTypeExpr :: TypeExpr -> TI ann TypeExpr
evalTypeExpr (GlobalTypeVar n tyExprs) = GlobalTypeVar n
                                         <$> mapM evalTypeExpr tyExprs
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

evalCtx :: Ctx -> TI ann Ctx
evalCtx = mapM evalTypeExpr

evalType :: Type -> TI ann Type
evalType (ctx', tyExpr) = (,) <$> evalCtx ctx' <*> evalTypeExpr tyExpr

evalDuctive :: Ductive -> TI ann Ductive
evalDuctive Ductive{..} = do
  gamma <- evalCtx gamma
  sigmas <- mapM (mapM evalExpr) sigmas
  as <- local (over tyCtx (++[gamma])) $ mapM evalTypeExpr as
  gamma1s <- mapM evalCtx gamma1s
  pure Ductive{..}

evalExpr :: Expr -> TI ann Expr
evalExpr r@Rec{..} = Rec <$> evalDuctive fromRec
                         <*> evalTypeExpr toRec
                         <*> mapM evalExpr matches
evalExpr r@Corec{..} = Corec <$> evalTypeExpr fromCorec
                             <*> evalDuctive toCorec
                             <*> mapM evalExpr matches
evalExpr (f :@: arg) = do
  valF <- evalExpr f
  valArg <- evalExpr arg
  case (valF, valArg) of
    -- TODO Should we check if _ = sigma_k\circ \tau
    (getExprArgs -> (r@Rec{..}, sigmaktau), getExprArgs -> (Constructor _ i, constrArgs))
      | length sigmaktau == length (gamma fromRec) -> do
      let gamma1 = gamma1s fromRec !! i
      recEval <- typeAction (as fromRec !! i)
                            [applyExprArgs (r, idCtx (gamma fromRec))
                            :@: LocalExprVar 0 ""]
                            [gamma1]
                            [In fromRec]
                            [toRec]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1)
                                   (1 + length gamma1)
                                   (substExprs 0 (reverse constrArgs)
                                                 (substExpr 0 recEval
                                                              (matches !! i)))
    (getExprArgs -> (Destructor ductive i, tau) , getExprArgs -> (c@Corec{..}, args))
      | length tau == length (gamma1s toCorec !! i) -> do
      let gamma1 = gamma1s toCorec !! i
      recEval <- typeAction (as toCorec !! i)
                            [applyExprArgs (c, idCtx (gamma toCorec))
                            :@: LocalExprVar 0 ""]
                            [gamma1]
                            [fromCorec]
                            [Coin toCorec]
      evalExpr $ shiftFreeVarsExpr ((-1) - length gamma1)
                                   (1 + length gamma1)
                                   (substExprs 0 (last args : reverse tau)
                                                 (substExpr 0 (matches !! i)
                                                              recEval))
    _ -> pure $ valF :@: valArg
evalExpr (GlobalExprVar v tyPars exprPars) =
  lookupDefExprTI v tyPars exprPars >>= evalExpr
evalExpr (WithParameters pars expr) = evalExpr $ substParsInExpr 0 (reverse pars) expr
evalExpr atom = pure atom

substExpr :: Int -> Expr -> Expr -> Expr
substExpr i r v@(LocalExprVar j _)
  | i == j = r
  | otherwise = v
substExpr i r (GlobalExprVar n tyPars exprPars) =
  GlobalExprVar n (map (substTypeExpr i r) tyPars)
                  (map (substExpr i r) exprPars)
substExpr i r (e1 :@: e2) = substExpr i r e1 :@: substExpr i r e2
substExpr i r re@Rec{..} = let Ductive{..} = fromRec
                               newMatches  = zipWith (\i m -> substExpr i (shiftFreeVarsExpr i 0 r) m)
                                                     (map ((+1) . (+i) . length) gamma1s)
                                                     matches
                           in re { matches = newMatches}
substExpr i r c@Corec{..} = let Ductive{..} = toCorec
                                newMatches  = zipWith (\i m -> substExpr i (shiftFreeVarsExpr i 0 r) m)
                                                      (map ((+1) . (+i) . length) gamma1s)
                                                      matches
                            in c { matches = newMatches }
substExpr _ _ e = e

substTypeExpr :: Int -> Expr -> TypeExpr -> TypeExpr
substTypeExpr i r (GlobalTypeVar n pars) = GlobalTypeVar n $ map (substTypeExpr i r) pars
substTypeExpr i r1 (Abstr t r2) =
  Abstr (substTypeExpr i r1 t)
        (substTypeExpr (i+1) (shiftFreeVarsExpr 0 1 r1) r2)
substTypeExpr i r1 (In d) = In $ substDuctiveExpr i r1 d
substTypeExpr i r1 (Coin d) = Coin $ substDuctiveExpr i r1 d
substTypeExpr i r (e1 :@ e2) = substTypeExpr i r e1 :@ substExpr i r e2
substTypeExpr _ _ e = e

substDuctiveExpr :: Int -> Expr -> Ductive -> Ductive
substDuctiveExpr i r1 dIn@Ductive{..} =
  let dOut = Ductive
        { gamma = substCtx i r1 gamma
        , sigmas =
            zipWith (\s g -> map (substExpr
                                     (1+i+length g)
                                     (shiftFreeVarsExpr 0
                                                        (1+i+length g)
                                                        r1))
                                     s)
                    sigmas gamma1s
        , as =
            zipWith (\g a -> substTypeExpr
                               (i + length g)
                               (shiftFreeVarsExpr 0
                                                  (i + length g)
                                                  r1)
                               a)
                    gamma1s as
        , gamma1s = map (substCtx i r1) gamma1s
        , nameDuc = "???"
        , strNames = []
        }
  in if dOut == dIn then dIn else dOut

substCtx :: Int -> Expr -> Ctx -> Ctx
substCtx _ _ [] = []
substCtx i r (e:ctx) =
  substTypeExpr i r e : substCtx (i+1) (shiftFreeVarsExpr 1 0 r) ctx

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
substType i r (GlobalTypeVar n vars) = GlobalTypeVar n $ map (substType i r) vars
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
  let dNew =  dOld { as = map (substType (i+1) (shiftFreeTypeVars 1 0 r1)) as
                   , nameDuc = "???" }
  in if dNew == dOld then dOld else dNew

substTypeInExpr :: Int -> TypeExpr -> Expr -> Expr
substTypeInExpr i r (e1 :@: e2) = substTypeInExpr i r e1 :@: substTypeInExpr i r e2
substTypeInExpr i r c@Constructor{..} = c {ductive = substDuctiveTypeExpr i r ductive }
substTypeInExpr i r d@Destructor{..} = d {ductive = substDuctiveTypeExpr i r ductive }
substTypeInExpr i r re@Rec{..} = re { fromRec = substDuctiveTypeExpr i r fromRec
                                    , toRec = substType i r toRec
                                    }
substTypeInExpr i r c@Corec{..} = c { fromCorec = substType i r fromCorec
                                    , toCorec = substDuctiveTypeExpr i r toCorec
                                    }
substTypeInExpr i r (WithParameters ps e) = WithParameters (map (substType i r) ps)
                                                           (substTypeInExpr i r e)
substTypeInExpr _ _ atom = atom

substTypesInExpr :: Int -> [TypeExpr] -> Expr -> Expr
substTypesInExpr _ [] e = e
substTypesInExpr n (v:vs) e = substTypesInExpr (n+1) vs (substTypeInExpr n v e)


substPar :: Int -> TypeExpr -> TypeExpr -> TypeExpr
substPar i r v@(Parameter j _)
  | i == j = r
  | otherwise = v
substPar i r (GlobalTypeVar n vars) = GlobalTypeVar n $ map (substPar i r) vars
substPar i r1 (Abstr t r2) = Abstr (substPar i r1 t) (substPar i r1 r2)
substPar i r (In d) = In $ substDuctivePar i r d
substPar i r (Coin d) = Coin $ substDuctivePar i r d
substPar i r (e1 :@ e2) = substPar i r e1 :@ substParInExpr i r e2
substPar _ _ e = e

substPars :: Int -> [TypeExpr] -> TypeExpr -> TypeExpr
substPars _ [] e = e
substPars n (v:vs) e = substPars (n+1) vs (substPar n v e)

substDuctivePar :: Int -> TypeExpr -> Ductive -> Ductive
substDuctivePar i r1 dOld@Ductive{..} =
  let dNew =  dOld { as = map (substPar i (shiftFreeTypeVars 1 0 r1)) as
                   , gamma =  substParInCtx i r1 gamma
                   , sigmas = map (map $ substParInExpr i (shiftFreeTypeVars 1 0 r1)) sigmas
                   , gamma1s = map (substParInCtx i r1) gamma1s
                   , nameDuc = "???" }
  in if dNew == dOld then dOld else dNew

substParInExpr :: Int -> TypeExpr -> Expr -> Expr
substParInExpr i r (e1 :@: e2) = substParInExpr i r e1 :@: substParInExpr i r e2
substParInExpr i r c@Constructor{..} = c {ductive = substDuctivePar i r ductive }
substParInExpr i r d@Destructor{..} = d {ductive = substDuctivePar i r ductive }
substParInExpr i r re@Rec{..} = re { fromRec = substDuctivePar i r fromRec
                                   , toRec = substPar i r toRec
                                   }
substParInExpr i r c@Corec{..} = c { fromCorec = substPar i r fromCorec
                                   , toCorec = substDuctivePar i r toCorec
                                   }
substParInExpr i r (WithParameters ps e) = substParInExpr (i+length ps) r e
substParInExpr _ _ atom = atom

substParsInExpr :: Int -> [TypeExpr] -> Expr -> Expr
substParsInExpr _ [] e = e
substParsInExpr n (v:vs) e = substParsInExpr (n+1) vs (substParInExpr n v e)

substParInCtx :: Int -> TypeExpr -> Ctx -> Ctx
substParInCtx i r = map (substPar i r)

substParInParCtx :: Int -> TypeExpr -> TyCtx -> TyCtx
substParInParCtx i r []          = []
substParInParCtx i r (ctx':ctxs) =
  substParInCtx i r ctx' : substParInParCtx (i+1) r ctxs

typeAction :: TypeExpr -> [Expr] -> [Ctx] -> [TypeExpr] -> [TypeExpr] -> TI ann Expr
typeAction (LocalTypeVar n _) terms _ _ _ = do
   l <- length <$> view parCtx
   pure $ terms !! (n + l)
typeAction (Parameter n _) terms _ _ _ = pure $ terms !! n
typeAction (GlobalTypeVar n vars) terms gammas as' bs  = do
  tyExpr <- lookupDefTypeExprTI n vars
  typeAction tyExpr terms gammas as' bs
typeAction (c :@ s) terms gammas as' bs = flip (substExpr 0) s
                                          <$> typeAction c terms gammas as' bs
typeAction (Abstr _ c) terms gammas as' bs = typeAction c terms gammas as' bs
typeAction (In d) terms gammas as' bs = do
  let idDeltas = map idCtx (gamma1s d)
      newAs = (map (substTypes 1 (zipWith abstrArgs as' gammas)) (as d))
      fromRec = d { as = newAs
                  , nameDuc = if newAs == as d
                              then nameDuc d
                              else "???"} -- R_a in paper
      toRec = In fromRec
      rb = applyDuctiveCtx IsIn fromRec
  matches <- sequence $ zipWith3 (\idDelta dk k -> do
    recEval <- typeAction dk
                          -- TODO is this the right variable?
                         -- Could be bound by wrong binder.
                         (LocalExprVar 0 "" : terms)
                         (gamma d : gammas)
                         (rb : as')
                         (rb : bs )
    pure $ applyExprArgs (Constructor fromRec k, idDelta)
                          :@: recEval) -- g_k in paper
                                  idDeltas (as d)  [0..]
  pure$  applyExprArgs (Rec {..}  , idCtx (gamma d)) :@: LocalExprVar 0 ""
typeAction (Coin d) terms gammas as' bs = do
  let idDeltas = map idCtx (gamma1s d)
      newAs = map (substTypes 1 (zipWith abstrArgs bs gammas)) (as d)
      toCorec = d { as = newAs
                  , nameDuc = if newAs == as d
                              then nameDuc d
                              else "???"} -- R_a in paper
      fromCorec = Coin toCorec
      rb = applyDuctiveCtx IsCoin toCorec
      -- g_k in paper
  matches <- sequence $ zipWith3 (\idDelta dk k -> do
    recEval <- typeAction dk
                          -- TODO is this the right variable?
                          -- Could be bound by wrong binder.
                          (LocalExprVar 0 "" : terms )
                          (gamma d : gammas )
                          (rb : as')
                          (rb : bs )
    pure $ substExpr 0 (applyExprArgs ( Destructor toCorec k
                                      , idDelta)
                        :@: LocalExprVar 0 "")
                       recEval)
                                 idDeltas (as d) [0..]
  pure $ applyExprArgs (Corec {..}  ,idCtx (gamma d)) :@: LocalExprVar 0 ""
typeAction _ _ _ _ _ = pure $ LocalExprVar 0 ""

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
idCtx [] = []
idCtx [_] = [LocalExprVar 0 ""]
idCtx ctx = map (flip LocalExprVar "") [length ctx - 1, length ctx - 2 .. 0]

-- | splits up a chain of left associative applications to a type into a
-- list of  arguments
getTypeExprArgs :: TypeExpr -> (TypeExpr,[Expr])
getTypeExprArgs (expr :@ arg) = second (++ [arg]) (getTypeExprArgs expr)
getTypeExprArgs arg = (arg,[])

applyTypeExprArgs :: (TypeExpr,[Expr]) -> TypeExpr
applyTypeExprArgs (f,args) = foldl (:@) f args

lookupLocalVarTI :: Int -> Text -> [a] -> TI ann a
lookupLocalVarTI i t ctx = lookupLocalVarTI' i (reverse ctx)
  where
    lookupLocalVarTI' :: Int -> [a] -> TI ann a
    lookupLocalVarTI' _ []     = throwError (pretty t <+> "not defined")
    lookupLocalVarTI' 0 (x:_)  = pure x
    lookupLocalVarTI' n (x:xs)
      | n < 0 = error "internal error negative variable lookup"
      | otherwise = lookupLocalVarTI' (n-1) xs

lookupDefTypeTI :: Text -- ^ name of expr var
                -> [TypeExpr] -- ^ type parameters to check
                -> [Expr] -- ^ expr parameters to check
                -> TI ann Type
lookupDefTypeTI t tyPars exprPars = view defCtx >>= lookupDefTypeTI'
  where
    lookupDefTypeTI' :: [Statement] -> TI ann Type
    lookupDefTypeTI' [] = throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefTypeTI' (ExprDef{..}:stmts)
      | t == name = do
          parsKinds <- mapM inferType tyPars
          catchError (betaeqTyCtx parsKinds tyParameterCtx)
                     (throwError . (<> "\n while looking up variable "
                                    <+> pretty t
                                    <+> "with type parameters"
                                    <+> pretty tyPars))
          parsTypes <- mapM inferTerm exprPars
          assert (all (null . fst) parsTypes)
                 "types expression parameters should have empty ctxts"
          catchError (betaeqCtx (map snd parsTypes)
                                (map (substPars 0 (reverse tyPars))
                                      exprParameterCtx))
                     (throwError . (<> "\n while looking up variable "
                                    <+> pretty t
                                    <+> "with expr parameters"
                                    <+> pretty exprPars))
          let (ctx',res) = fromJust ty
          pure ( map (substPars 0 (reverse tyPars)) ctx'
               , substPars 0 (reverse tyPars) res)
      | otherwise = lookupDefTypeTI' stmts
    lookupDefTypeTI' (_:stmts) = lookupDefTypeTI' stmts

lookupDefExprTI :: Text -- ^ name of expr var
                -> [TypeExpr] -- ^ type parameters to check
                -> [Expr] -- ^ expr parameters to check
                -> TI ann Expr
lookupDefExprTI t tyPars exprPars = view defCtx >>= lookupDefExprTI'
  where
    lookupDefExprTI' :: [Statement] -> TI ann Expr
    lookupDefExprTI' [] = throwError $ "Variable" <+> pretty t <> "not defined"
    lookupDefExprTI' (ExprDef{..}:stmts)
      | t == name = do
          assert (length tyPars == length tyParameterCtx)
                 ("parameters to type variables have to be complete"
                 <> "\n while looking up" <+> pretty t
                 <> "\n with parameters" <+> pretty tyPars
                 <> "\n and parameter context" <+> pretty tyParameterCtx)
          assert (length exprPars == length exprParameterCtx)
                 ("parameters to type variables have to be complete"
                 <> "\n while looking up" <+> pretty t
                 <> "\n with parameters" <+> pretty exprPars
                 <> "\n and parameter context" <+> pretty exprParameterCtx)
          pure $ substParsInExpr 0 (reverse tyPars)
               $ substExprs 0 (reverse exprPars) expr
      | otherwise = lookupDefExprTI' stmts
    lookupDefExprTI' (_:stmts) = lookupDefExprTI' stmts

lookupDefKindTI :: Text -- ^ name of type var
                -> [TypeExpr] -- ^ parameters to check
                -> TI ann Kind
lookupDefKindTI t pars = view defCtx >>= lookupDefKindTI'
  where
    lookupDefKindTI' :: [Statement] -> TI ann Kind
    lookupDefKindTI' [] = throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefKindTI' (TypeDef{..}:stmts)
      | t == name = do
          catchError (checkParKinds pars parameterCtx)
                     (throwError . (<> "\n while looking up variable "
                                    <+> pretty t
                                    <+> "with parameters"
                                    <+> pretty pars))
          pure (fromJust kind)
      | otherwise = lookupDefKindTI' stmts
    lookupDefKindTI' (_:stmts) = lookupDefKindTI' stmts

checkParKinds :: [TypeExpr] -> TyCtx -> TI ann ()
checkParKinds [] [] = pure ()
checkParKinds (par:pars) (ctx':ctxs) = do
  kind <- local (ctx .~ []) $ inferType par
  betaeqCtx ctx' kind
  checkParKinds pars (substParInParCtx 0 par ctxs)
checkParKinds _ _ = throwError "number of parameters doesn't match the expected one"

lookupDefTypeExprTI :: Text -- ^ name of type var
                    -> [TypeExpr] -- ^ parameters to check
                    -> TI ann TypeExpr
lookupDefTypeExprTI t pars = view defCtx >>= lookupDefTypeExprTI'
  where
    lookupDefTypeExprTI' :: [Statement] -> TI ann TypeExpr
    lookupDefTypeExprTI' [] = throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefTypeExprTI' (TypeDef{..}:stmts)
      | t == name = do
          assert (length pars == length parameterCtx)
                 ("parameters to type variables have to be complete"
                 <> "\n while looking up" <+> pretty t
                 <> "\n with parameters" <+> pretty pars
                 <> "\n and parameter context" <+> pretty parameterCtx)
--    TODO This function gets used in evalution with doesn't change the context
--         Are this tests really not necessary?
--        parsKinds <- mapM inferType pars
--        betaeqTyCtx parsKinds parameterCtx
          pure $ substPars 0 (reverse pars) typeExpr
      | otherwise = lookupDefTypeExprTI' stmts
    lookupDefTypeExprTI' (_:stmts) = lookupDefTypeExprTI' stmts

assert :: Bool -> Doc ann -> TI ann ()
assert True  _   = pure ()
assert False msg = throwError msg

shiftFreeVarsCtx :: Int -> Int -> Ctx -> Ctx
shiftFreeVarsCtx _ _ []       = []
shiftFreeVarsCtx j k (ty:tys) =
  shiftFreeVarsTypeExpr j k ty : shiftFreeVarsCtx j (k+1) tys

shiftFreeVarsTypeExpr :: Int -- ^ how much should they be shifted
                      -> Int -- ^ offset for free vars
                      -> TypeExpr -> TypeExpr
shiftFreeVarsTypeExpr j k (e1 :@ e2) = e1 :@ shiftFreeVarsExpr j k e2
shiftFreeVarsTypeExpr j k (GlobalTypeVar n vars) =
  GlobalTypeVar n $ map (shiftFreeVarsTypeExpr j k) vars
shiftFreeVarsTypeExpr j k (Abstr ty body) =
  Abstr (shiftFreeVarsTypeExpr j k ty) (shiftFreeVarsTypeExpr j (k+1) body)
shiftFreeVarsTypeExpr _ _ e = e

shiftFreeTypeVars :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> TypeExpr -> TypeExpr
shiftFreeTypeVars j k v@(LocalTypeVar i n)
  | i < k = v
  | otherwise = LocalTypeVar (i+j) n
shiftFreeTypeVars j k (e1 :@ e2) = shiftFreeTypeVars j k e1 :@ e2
shiftFreeTypeVars j k (GlobalTypeVar n vars) =
  GlobalTypeVar n $ map (shiftFreeTypeVars j k) vars
shiftFreeTypeVars j k (Abstr ty body) =
  Abstr (shiftFreeTypeVars j k ty) (shiftFreeTypeVars j k body)
shiftFreeTypeVars j k (In d) = In $ shiftFreeTypeVarsDuc j k d
shiftFreeTypeVars j k (Coin d) = Coin $ shiftFreeTypeVarsDuc j k d
shiftFreeTypeVars _ _ e = e


shiftFreeTypeVarsDuc :: Int -- ^ how much should they be shifted
                     -> Int -- ^ offset for free vars
                     -> Ductive -> Ductive
shiftFreeTypeVarsDuc j k d = d { as = map (shiftFreeTypeVars j (k+1)) (as d) }

shiftFreeVarsExpr :: Int -- ^ how much should they be shifted
                  -> Int -- ^ offset for free vars
                  -> Expr -> Expr
shiftFreeVarsExpr j k v@(LocalExprVar i n)
  | i < k = v
  | otherwise = LocalExprVar (i+j) n
-- TODO maybe GlobaleExprVar
shiftFreeVarsExpr j k (e1 :@: e2) = shiftFreeVarsExpr j k e1
                                    :@: shiftFreeVarsExpr j k e2
shiftFreeVarsExpr j k r@Rec{..} =
  r {matches = zipWith (shiftFreeVarsExpr j) (map ( (1+) . (k+) . length)
                                             (gamma1s fromRec))
                       matches }
shiftFreeVarsExpr j k c@Corec{..} =
  c {matches = zipWith (shiftFreeVarsExpr j) (map ( (1+) . (k+). length)
                                             (gamma1s toCorec))
                       matches }
shiftFreeVarsExpr _ _ e = e

shiftFreeParsKind :: Int -> Int -> Kind -> Kind
shiftFreeParsKind j k = map (shiftFreeParsTypeExpr j k)

shiftFreeParsTypeExpr :: Int -> Int -> TypeExpr -> TypeExpr
shiftFreeParsTypeExpr j k p@(Parameter i n)
  | i < k = p
  | otherwise = Parameter (i+j) n
shiftFreeParsTypeExpr j k expr = overTypeExpr (shiftFreeParsTypeExpr j k)
                                              (shiftFreeParsDuc j k)
                                              (shiftFreeParsExpr j k)
                                              expr

shiftFreeParsDuc :: Int -> Int -> Ductive -> Ductive
shiftFreeParsDuc j k = overDuctive (shiftFreeParsTypeExpr j k)
                                   (shiftFreeParsExpr j k)

shiftFreeParsExpr :: Int -> Int -> Expr -> Expr
shiftFreeParsExpr j k (WithParameters pars expr) =
  WithParameters (map (shiftFreeParsTypeExpr j k) pars)
                 (shiftFreeParsExpr j (k + length pars) expr)
shiftFreeParsExpr j k expr = overExpr (shiftFreeParsTypeExpr j k)
                                      (shiftFreeParsDuc j k)
                                      (shiftFreeParsExpr j k)
                                      expr

overTypeExpr :: (TypeExpr -> TypeExpr)
             -> (Ductive -> Ductive)
             -> (Expr -> Expr)
             -> TypeExpr -> TypeExpr
overTypeExpr fTyExpr fDuc fExpr =
  runIdentity . overTypeExprM (pure . fTyExpr)
                              (pure . fDuc)
                              (pure . fExpr)

overTypeExprM :: Monad m
              => (TypeExpr -> m TypeExpr)
              -> (Ductive -> m Ductive)
              -> (Expr -> m Expr)
              -> TypeExpr -> m TypeExpr
overTypeExprM fTyExpr _ fExpr (tyExpr :@ expr) =
  (:@) <$> fTyExpr tyExpr <*> fExpr expr
overTypeExprM fTyExpr _ _ (GlobalTypeVar n tyExprs) =
  GlobalTypeVar n <$> mapM fTyExpr tyExprs
overTypeExprM fTyExpr _ _ (Abstr tyExpr1 tyExpr2) =
  Abstr <$> fTyExpr tyExpr1 <*> fTyExpr tyExpr2
overTypeExprM _ fDuc _ (In duc) = In <$> fDuc duc
overTypeExprM _ fDuc _ (Coin duc) = Coin <$> fDuc duc
overTypeExprM fTyExpr _ _ atom = fTyExpr atom

overDuctive :: (TypeExpr -> TypeExpr)
             -> (Expr -> Expr)
             -> Ductive -> Ductive
overDuctive fTyExpr fExpr =
  runIdentity . overDuctiveM (pure . fTyExpr) (pure . fExpr)

overDuctiveM :: Monad m
             => (TypeExpr -> m TypeExpr)
             -> (Expr -> m Expr)
             -> Ductive -> m Ductive
overDuctiveM fTyExpr fExpr Ductive{..} = do
  gamma <- overTypeExprInCtxM fTyExpr gamma
  sigmas <- mapM (mapM fExpr) sigmas
  as <- mapM fTyExpr as
  gamma1s <- mapM (overTypeExprInCtxM fTyExpr) gamma1s
  pure Ductive{..}

overTypeExprInCtxM :: Monad m => (TypeExpr -> m TypeExpr) -> Ctx -> m Ctx
overTypeExprInCtxM = mapM

overExpr :: (TypeExpr -> TypeExpr)
         -> (Ductive -> Ductive)
         -> (Expr -> Expr)
         -> Expr -> Expr
overExpr fTyExpr fDuc fExpr =
  runIdentity . overExprM (pure . fTyExpr)
                          (pure . fDuc)
                          (pure . fExpr)

overExprM :: Monad m
          => (TypeExpr -> m TypeExpr)
          -> (Ductive -> m Ductive)
          -> (Expr -> m Expr)
          -> Expr -> m Expr
overExprM fTyExpr _ fExpr (GlobalExprVar n tyExprs exprs) =
  GlobalExprVar n <$> mapM fTyExpr tyExprs <*> mapM fExpr exprs
overExprM _ _ fExpr (expr1 :@: expr2) = (:@:) <$> fExpr expr1
                                              <*> fExpr expr2
overExprM _ fDuc _ Constructor{..} = do
  ductive <- fDuc ductive
  pure Constructor{..}
overExprM _ fDuc _ Destructor{..} = do
  ductive <- fDuc ductive
  pure Destructor{..}
overExprM fTyExpr fDuc fExpr Rec{..} = do
  fromRec <- fDuc fromRec
  toRec <- fTyExpr toRec
  matches <- mapM fExpr matches
  pure Rec{..}
overExprM fTyExpr fDuc fExpr Corec{..} = do
  fromCorec <- fTyExpr fromCorec
  toCorec <- fDuc toCorec
  matches <- mapM fExpr matches
  pure Corec{..}
overExprM fTyExpr _ fExpr (WithParameters pars expr) =
  WithParameters <$> mapM fTyExpr pars <*> fExpr expr

data InOrCoin = IsIn | IsCoin

-- | apply the context of a ductive to it
applyDuctiveCtx :: InOrCoin -> Ductive -> TypeExpr
applyDuctiveCtx IsIn   d@Ductive{..} = applyTypeExprArgs (In d, idCtx gamma)
applyDuctiveCtx IsCoin d@Ductive{..} = applyTypeExprArgs (Coin d, idCtx gamma)

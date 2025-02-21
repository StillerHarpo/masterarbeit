{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Lens.Micro.Platform

import           Data.Text                      (Text)
import qualified Data.Text                 as T

import           Data.Maybe                     (fromJust)

import           Data.Text.Prettyprint.Doc

import           AbstractSyntaxTree
import           ShiftFreeVars
import           Mark
import           Subst
import           TypeAction
import           Eval
import           Betaeq

-- | Type inference monad
type TI ann = ExceptT (Doc ann) (Reader ContextTI)

data ContextTI = ContextTI { _ctx    :: Ctx
                           , _tyCtx  :: TyCtx
                           , _parCtx :: TyCtx
                           , _defCtx :: [Decl]
                           }
$(makeLenses ''ContextTI)

runTI :: TI ann a -> ContextTI -> Either (Doc ann) a
runTI ti = runReader (runExceptT ti)

-- | Program type inference monad
type PTI ann = ExceptT (Doc ann) (State [Decl])

runPTI :: PTI ann a -> [Decl] -> (Either (Doc ann) a, [Decl])
runPTI pti = runState (runExceptT pti)

evalPTI :: PTI ann a -> [Decl] -> Either (Doc ann) a
evalPTI pti = evalState (runExceptT pti)

emptyCtx :: ContextTI
emptyCtx = ContextTI { _ctx = []
                     , _tyCtx = []
                     , _parCtx = []
                     , _defCtx = []}

checkProgram :: [Decl] -> Either (Doc ann) [TypedExpr]
checkProgram = flip evalPTI [] . checkProgramPTI

checkProgramPTI :: [Decl] -> PTI ann [TypedExpr]
checkProgramPTI []                       = pure []
checkProgramPTI (ExprDef{..} : decls)    = do
  tiInPTI $ checkParCtx tyParameterCtx
  tiInPTI $ local (set parCtx tyParameterCtx) $ checkCtx exprParameterCtx
  ty <- Just <$> tiInPTI (local (set parCtx tyParameterCtx
                                 . set ctx exprParameterCtx)
                                (inferTerm expr
                                 >>= (evalInTI . evalType)))
  -- expr <- evalInPTI $ evalExpr expr
  modify (ExprDef{..} :)
  checkProgramPTI decls
checkProgramPTI (TypeDef duc : decls)    =
  tiInPTI (checkTypeDuctive duc)
  >> modify (TypeDef duc :)
  >> checkProgramPTI decls
checkProgramPTI (Expression expr : decls) =
  (:) <$> (TypedExpr <$> evalInPTI (evalExpr expr)
                     <*> tiInPTI (inferTerm expr
                                  >>= (evalInTI . evalType)))
      <*> checkProgramPTI decls

tiInPTI :: TI ann a -> PTI ann a
tiInPTI ti = do
  curDefCtx <- get
  case runTI ti $ set defCtx curDefCtx emptyCtx of
    Left err -> throwError err
    Right a  -> pure a

evalInTI :: Eval ann a -> TI ann a
evalInTI eval = do
  declCtx <- view defCtx
  numPars <- length <$> view parCtx
  case runEval eval EvalCtx {..} of
    Left err -> throwError err
    Right a  -> pure a

evalInPTI :: Eval ann a -> PTI ann a
evalInPTI = tiInPTI . evalInTI

checkTyCtx :: TyCtx -> TI ann ()
checkTyCtx = mapM_ checkCtx

checkParCtx :: TyCtx -> TI ann ()
checkParCtx parCtx' =
  catchError (checkParCtx' parCtx')
             (throwError . (<> "\n in parameter context"
                           <+> pretty parCtx'))
    where
      checkParCtx' []             =
        pure ()
      checkParCtx' (ctx':parCtx') =
        checkCtx ctx'
        >> local (over parCtx (ctx':)) (checkParCtx parCtx')

checkCtx :: Ctx -> TI ann ()
checkCtx ctx' = catchError (checkCtx' ctx')
                           (throwError . (<> "\n in context"
                                          <+> pretty ctx'))
  where
    checkCtx' :: Ctx -> TI ann ()
    checkCtx' []         =
      pure ()
    checkCtx' (typ:ctx') = do
      local (over ctx (++[typ])) (checkCtx' ctx')
      local (set tyCtx []) (checkType typ [])

checkType :: TypeExpr -> Kind -> TI ann ()
checkType e k = inferType e >>= (evalInTI . betaeqCtx k)

inferType :: TypeExpr -> TI ann Kind
inferType tyExpr = catchError (inferType' tyExpr)
                              (throwError . (<> "\n in type expression "
                                             <+> pretty tyExpr))
  where
    inferType' UnitType               =
      pure []
    inferType' v@(LocalTypeVar idx _ _) =
      do ty <- view tyCtx >>= lookupLocalVarTI idx (T.pack $ show v)
         --view ctx >>= checkCtx
         pure ty
    inferType' v@(Parameter idx _ _)   =
      do ty <- view parCtx >>= lookupLocalVarTI idx (T.pack $ show v)
         --view ctx >>= checkCtx
         pure (shiftFreeParsKind (idx + 1) 0 ty)
    inferType' (GlobalTypeVar n pars) =
      lookupDefKindTI n pars
    inferType' (a :@ t)               =
      inferType a >>= \case
        [] -> throwError "Can't apply someting to a type with a empty context"
        (b:gamma2) -> do
          (ctx,b') <- inferTerm t
          assert (null ctx) "Type Ctx should be empty"
          evalInTI $ betaeq b b'
          pure $ substCtx 0 t gamma2
    inferType' (Abstr _ tyX b)        =
      (tyX:) <$> local (ctx %~ (++[tyX])) (inferType b)
    inferType' Ductive{..}            =
      checkParKinds parametersTyExpr (parameterCtx openDuctive)
      >> local (ctx .~ [])
               (inferTypeDuctiveWithPars parametersTyExpr openDuctive)

inferTypeDuctiveWithPars :: [TypeExpr] -> OpenDuctive -> TI ann Kind
inferTypeDuctiveWithPars pars od@OpenDuctive{..} =
  checkParKinds pars parameterCtx
  >> checkTypeDuctive od
  >> pure (substParsInCtx 0 pars gamma)

checkTypeDuctive :: OpenDuctive -> TI ann ()
checkTypeDuctive OpenDuctive{..} = do
  checkParCtx parameterCtx
  local (over parCtx (parameterCtx++))
        (mapM_ checkStrDef strDefs)
    where
      checkStrDef :: StrDef -> TI ann ()
      checkStrDef StrDef{..} =
        checkContextMorph sigma gamma1 gamma
        >> local (over tyCtx (++[gamma]) . set ctx gamma1)
                 (checkType a [])


checkContextMorph :: [Expr] -> Ctx -> Ctx -> TI ann ()
checkContextMorph exprs gamma1 gamma2 =
  catchError (checkContextMorph' exprs gamma1 gamma2)
             (throwError . (<> "\n in context morphismus "
                            <+> pretty exprs))
  where
    checkContextMorph' []     gamma1 []         =
      checkCtx gamma1
    checkContextMorph' (t:ts) gamma1 (a:gamma2) =
      do local (over ctx (const gamma1))
               (checkTerm t ([], substTypeExprs 0 ts a))
         checkContextMorph ts gamma1 gamma2
    checkContextMorph' _      _      _          =
      throwError $ "Invalid context morphism:"
                   <> "Gamma2 should be empty for empty morphism"

checkTerm :: Expr -> Type -> TI ann ()
checkTerm e (ctx1,a1) = do
  (ctx2, a2) <- inferTerm e
  evalInTI $ betaeqCtx ctx1 ctx2
  evalInTI $ betaeq a1 a2

inferTerm :: Expr -> TI ann Type
inferTerm expr = catchError (inferTerm' expr)
                                (throwError . (<> "\n in expression"
                                               <+> pretty expr))
  where
    inferTerm' UnitExpr = pure ([],UnitType)
    inferTerm' v@(LocalExprVar idx _ _)          =
      ([],) . shiftFreeVarsTypeExpr (idx + 1) 0
      <$> (view ctx >>= lookupLocalVarTI idx (T.pack $ show v))
    inferTerm' (GlobalExprVar x tyPars exprPars) =
      lookupDefTypeTI x tyPars exprPars
    inferTerm' (t :@: s)                         =
      inferTerm t >>= \case
        ([],_) -> throwError "Can't apply something to a term with a empty context"
        (a:ctx2,b) -> do
          (ctx,a') <- inferTerm s
          assert (null ctx) "Type Ctx should be empty"
          evalInTI $ betaeq a a'
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
    inferTerm' Structor{..}                     =
      inferTypeDuctiveWithPars parameters ductive
      >> pure (let StrDef{..} = strDefs ductive !! num
                   openDuctive = ductive
                   parametersTyExpr = parameters
               in case inOrCoin ductive of
                  IsIn -> ( substParsInCtx 0 (reverse parameters) gamma1
                            ++ [substType 0 Ductive{..}
                                $ substPars 0 (reverse parameters) a]
                          , shiftFreeVarsTypeExpr 1 0
                            $ applyTypeExprArgs (Ductive{..} , sigma))
                  IsCoin -> ( substParsInCtx 0 (reverse parameters) gamma1
                              ++ [applyTypeExprArgs (Ductive{..}, sigma)]
                            , shiftFreeVarsTypeExpr 1 0
                              $ substType 0 Ductive{..}
                              $ substPars 0 (reverse parameters) a))
    inferTerm' Iter{..}                         = do
      motive <- evalInTI $  evalTypeExpr motive
      -- TODO do we have to disallow free variables here
      ductive <- evalInTI $ evalDuctive ductive
      gamma' <- inferType motive
      void $ inferTypeDuctiveWithPars parameters ductive
      let OpenDuctive{..} = ductive
          openDuctive = ductive
          parametersTyExpr = parameters
      evalInTI $ betaeqCtx gamma' gamma
      case inOrCoin of
        IsIn -> do
         zipWithM_ (\StrDef{..} match ->
                      local (over ctx (++ substParsInCtx 0 (reverse parameters) gamma1
                                       ++[substType 0 (shiftFreeVarsTypeExpr
                                                         (length gamma1)
                                                         0 motive)
                                          $ substPars 0 (reverse parameters) a ]))
                            (catchError
                              (checkTerm
                                 match
                                 ([],applyTypeExprArgs ( shiftFreeVarsTypeExpr
                                                           (length gamma1 + 1)
                                                           0 motive
                                                       , map (shiftFreeVarsExpr 1 0)
                                                             sigma)))
                              (throwError . (<> "\n in match" <+> pretty match))))
                   strDefs (map snd matches)
         pure ( substParsInCtx 0 (reverse parameters) gamma
                ++ [applyTypeExprArgs (Ductive{..}, idCtx gamma)]
              , applyTypeExprArgs ( motive
                                  , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))
        IsCoin -> do
          evalInTI $ betaeqCtx gamma' gamma
          zipWithM_ (\StrDef{..} match ->
                        local (over ctx (++ substParsInCtx 0 (reverse parameters) gamma1
                                         ++[applyTypeExprArgs ( shiftFreeVarsTypeExpr
                                                                  (length gamma1)
                                                                  0 motive
                                                              , sigma)]))
                              (catchError
                                (checkTerm match ([], shiftFreeVarsTypeExpr 1 0
                                                    $ substType 0 (shiftFreeVarsTypeExpr
                                                                     (length gamma1 + 1)
                                                                     0
                                                                     motive)
                                                    $ substPars 0 (reverse parameters) a))
                                (throwError . (<> "\n in match" <+> pretty match))))
                    strDefs (map snd matches)
          pure ( substParsInCtx 0 (reverse parameters) gamma
                 ++ [applyTypeExprArgs (motive, idCtx gamma)]
               , applyTypeExprArgs ( Ductive{..}
                                   , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))

lookupLocalVarTI :: Int -> Text -> [a] -> TI ann a
lookupLocalVarTI i t ctx = lookupLocalVarTI' i (reverse ctx)
  where
    lookupLocalVarTI' :: Int -> [a] -> TI ann a
    lookupLocalVarTI' _ []     = throwError (pretty t <+> "not defined")
    lookupLocalVarTI' 0 (x:_)  = pure x
    lookupLocalVarTI' n (_:xs)
      | n < 0                  =
          error "internal error negative variable lookup"
      | otherwise              = lookupLocalVarTI' (n-1) xs

lookupDefTypeTI :: Text -- ^ name of expr var
                -> [TypeExpr] -- ^ type parameters to check
                -> [Expr] -- ^ expr parameters to check
                -> TI ann Type
lookupDefTypeTI t tyPars exprPars = view defCtx >>= lookupDefTypeTI'
  where
    lookupDefTypeTI' :: [Decl] -> TI ann Type
    lookupDefTypeTI' []                  =
         throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefTypeTI' (ExprDef{..}:decls)
      | t == name                        =
          do catchError (tyExprsForParCtx tyPars tyParameterCtx)
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with type parameters"
                                       <+> pretty tyPars))
             catchError (exprsForCtx exprPars
                                     (map (substPars 0 (reverse tyPars))
                                          exprParameterCtx))
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with expr parameters"
                                       <+> pretty exprPars))
             let (ctx',res) = fromJust ty
             pure ( map (substPars 0 (reverse tyPars))
                    $ substExprsInCtx 0 (reverse exprPars) ctx'
                  , substPars 0 (reverse tyPars)
                    $ substTypeExprs (length ctx') (reverse exprPars) res)
      | otherwise                        =
          lookupDefTypeTI' decls
    lookupDefTypeTI' (_:decls)           =
          lookupDefTypeTI' decls

tyExprsForParCtx :: [TypeExpr] -> TyCtx -> TI ann ()
tyExprsForParCtx [] [] = pure ()
tyExprsForParCtx (tyExprPar:tyExprPars) (kind:kinds) = do
  parKind <- inferType tyExprPar
  evalInTI $ betaeqCtx parKind kind
  tyExprsForParCtx tyExprPars
                   (map (substParInCtx 0 (markInTyExpr tyExprPar))
                        kinds)
tyExprsForParCtx _      _        =
  throwError "length of parameters does not match expected one"

exprsForCtx :: [Expr] -> Ctx -> TI ann ()
exprsForCtx []                 []       = pure ()
exprsForCtx (exprPar:exprPars) (ty':tys) = do
  parsType <- inferTerm exprPar
  assert (null $ fst parsType)
          "types expression parameters should have empty ctxts"
  evalInTI $ betaeq (snd parsType) ty'
  exprsForCtx  exprPars (substCtxNoShift 0 exprPar tys)
    where
      -- subst in context but doesn't shift vars
      substCtxNoShift :: Int -> Expr -> Ctx -> Ctx
      substCtxNoShift _ _ []        = []
      substCtxNoShift i e (ty':tys) = substTypeExpr i e ty'
                                      : substCtxNoShift (i+1) e tys
exprsForCtx _      _        =
  throwError "length of parameters does not match expected one"

lookupDefKindTI :: Text -- ^ name of type var
                -> [TypeExpr] -- ^ parameters to check
                -> TI ann Kind
lookupDefKindTI t pars = view defCtx >>= lookupDefKindTI'
  where
    lookupDefKindTI' :: [Decl] -> TI ann Kind
    lookupDefKindTI' []                  =
          throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefKindTI' (TypeDef OpenDuctive{..}:decls)
      | t == nameDuc                     =
          do catchError (checkParKinds pars parameterCtx)
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with parameters"
                                       <+> pretty pars))
             pure $ map (substPars 0 (reverse pars)) gamma
      | otherwise                        =
          lookupDefKindTI' decls
    lookupDefKindTI' (_:decls)           =
          lookupDefKindTI' decls

checkParKinds :: [TypeExpr] -> TyCtx -> TI ann ()
checkParKinds []         []          =
  pure ()
checkParKinds (par:pars) (ctx':ctxs) =
  do kind <- local (ctx .~ []) $ inferType par
     evalInTI $ betaeqCtx ctx' kind
     checkParKinds pars (substParInParCtx 0 par ctxs)
checkParKinds _          _           =
  throwError "number of parameters doesn't match the expected one"

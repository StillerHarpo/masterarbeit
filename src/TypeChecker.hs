{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language RecordWildCards #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module TypeChecker where

import           Debug.Trace

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List

import           Lens.Micro.Platform

import           Data.Text                      (Text)
import qualified Data.Text                 as T

import           Data.Maybe                     (fromJust)

import           Data.Text.Prettyprint.Doc

import           AbstractSyntaxTree
import           ShiftFreeVars
import           Subst
import           TypeAction
import           Eval
import           Betaeq

import           PrettyPrinter

-- | Type inference monad
type TI ann = ExceptT (Doc ann) (Reader ContextTI)

data ContextTI = ContextTI { _ctx    :: Ctx
                           , _tyCtx  :: TyCtx
                           , _parCtx :: TyCtx
                           , _defCtx :: [Statement]
                           }
$(makeLenses ''ContextTI)

runTI :: TI ann a -> ContextTI -> Either (Doc ann) a
runTI ti = runReader (runExceptT ti)

-- | Program type inference monad
type PTI ann = ExceptT (Doc ann) (State [Statement])

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
checkProgramPTI []                       = pure []
checkProgramPTI (ExprDef{..} : stmts)    = do
  tiInPTI $ checkParCtx tyParameterCtx
  tiInPTI $ local (set parCtx tyParameterCtx) $ checkCtx exprParameterCtx
  ty <- Just <$> tiInPTI (local (set parCtx tyParameterCtx
                                 . set ctx exprParameterCtx)
                                (inferTerm expr
                                 >>= (evalInTI . evalType)))
  expr <- evalInPTI $ evalExpr expr
  modify (ExprDef{..} :)
  checkProgramPTI stmts
checkProgramPTI (TypeDef{..} : stmts)    = do
  tiInPTI $ checkParCtx parameterCtx
  kind <- Just <$> tiInPTI (local (set parCtx parameterCtx)
                                  (inferType typeExpr
                                   >>= (evalInTI .evalCtx)))
  typeExpr <- evalInPTI $ evalTypeExpr typeExpr
  modify (TypeDef{..} :)
  checkProgramPTI stmts
checkProgramPTI (Expression expr : stmts) =
  (:) <$> (TypedExpr <$> evalInPTI (evalExpr expr)
                     <*> tiInPTI (inferTerm expr
                                  >>= (evalInTI . evalType)))
      <*> checkProgramPTI stmts

tiInPTI :: TI ann a -> PTI ann a
tiInPTI ti = do
  curDefCtx <- get
  case runTI ti $ set defCtx curDefCtx emptyCtx of
    Left err -> throwError err
    Right a  -> pure a

evalInTI :: Eval ann a -> TI ann a
evalInTI eval = do
  stmtCtx <- view defCtx
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
    checkCtx' []         =
      pure ()
    checkCtx' (typ:ctx') =
      local (over tyCtx (const []) . over ctx (const ctx'))
            (checkType typ [])
      >> checkCtx ctx'

checkType :: TypeExpr -> Kind -> TI ann ()
checkType e k = inferType e >>= (evalInTI . betaeqCtx k)

inferType :: TypeExpr -> TI ann Kind
inferType tyExpr = catchError (inferType' tyExpr)
                              (throwError . (<> "\n in type expression "
                                             <+> pretty tyExpr))
  where
    inferType' UnitType               =
      pure []
    inferType' v@(LocalTypeVar idx _) =
      do ty <- view tyCtx >>= lookupLocalVarTI idx (T.pack $ show v)
         view ctx >>= checkCtx
         pure ty
    inferType' v@(Parameter idx _)    =
      do ty <- view parCtx >>= lookupLocalVarTI idx (T.pack $ show v)
         view ctx >>= checkCtx
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
    inferType' (Abstr tyX b)          =
      (tyX:) <$> local (ctx %~ (tyX:)) (inferType b)
    inferType' (In d)                 =
      local (ctx .~ []) (inferTypeDuctive d)
    inferType' (Coin d)               =
      local (ctx .~ []) (inferTypeDuctive d)

inferTypeDuctive :: Ductive -> TI ann Kind
inferTypeDuctive Ductive{..} = mapM_ checkStrDef strDefs >> pure gamma
  where
    checkStrDef :: StrDef -> TI ann ()
    checkStrDef StrDef{..} =
      checkContextMorph sigma gamma1 gamma
      >> local (over tyCtx (++[gamma]) . set ctx gamma1) (checkType a [])

checkContextMorph :: [Expr] -> Ctx -> Ctx -> TI ann ()
checkContextMorph exprs gamma1 gamma2 =
  catchError (checkContextMorph' exprs gamma1 gamma2)
             (throwError . (<> "\n in context morphismus "
                            <+> pretty exprs))
  where
    checkContextMorph' []     gamma1 []         =
      checkCtx gamma1
    checkContextMorph' []     _      _          =
      throwError $ "Invalid context morphism:"
                   <> "Gamma2 should be empty for empty morphism"
    checkContextMorph' (t:ts) gamma1 (a:gamma2) =
      do local (over ctx (const gamma1))
               (checkTerm t ([], substTypeExprs 0 ts a))
         checkContextMorph ts gamma1 gamma2

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
    inferTerm' v@(LocalExprVar idx _)            =
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
    inferTerm' (Constructor d@Ductive{..} i)    =
      inferTypeDuctive d
      >> pure (let StrDef{..} = strDefs !! i
               in ( gamma1 ++ [substType 0 (In d) a]
                  , shiftFreeVarsTypeExpr 1 0
                    $ applyTypeExprArgs (In d , sigma)))
    inferTerm' (Destructor d@Ductive{..} i)     =
      inferTypeDuctive d
      >> pure (let StrDef{..} = strDefs !! i
               in ( gamma1 ++ [applyTypeExprArgs (Coin d, sigma)]
                  , shiftFreeVarsTypeExpr 1 0 $ substType 0 (Coin d) a))
    inferTerm' Rec{..}                          = do
      valTo <- evalInTI $  evalTypeExpr toRec
      valFrom <- evalInTI $ evalDuctive fromRec
      gamma' <- inferType valTo
      let Ductive{..} = valFrom
      evalInTI $ betaeqCtx gamma' gamma
      zipWithM_ (\StrDef{..} match ->
                   local (over ctx (++gamma1++[substType 0 valTo a]))
                         (checkTerm match ([],applyTypeExprArgs (valTo,sigma))))
                strDefs matches
      pure ( gamma ++ [applyTypeExprArgs (In valFrom, idCtx gamma)]
           , applyTypeExprArgs ( valTo
                               , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))
    inferTerm' Corec{..}                       = do
      valTo <- evalInTI $ evalDuctive toCorec
      valFrom <-evalInTI $  evalTypeExpr fromCorec
      gamma' <- inferType valFrom
      let Ductive{..} = valTo
      evalInTI $ betaeqCtx gamma' gamma
      zipWithM_ (\StrDef{..} match ->
                    local (over ctx (++gamma1++[applyTypeExprArgs (valFrom,sigma)]))
                          (checkTerm match ([], shiftFreeVarsTypeExpr 1 0 $ substType 0 valFrom a)))
                strDefs matches
      pure ( gamma ++ [applyTypeExprArgs (valFrom, idCtx gamma)]
           , applyTypeExprArgs (Coin valTo
                               , map (shiftFreeVarsExpr 1 0) (idCtx gamma)))
    inferTerm' (WithParameters ps e) = inferTerm $ substParsInExpr 0 (reverse ps) e

lookupLocalVarTI :: Int -> Text -> [a] -> TI ann a
lookupLocalVarTI i t ctx = lookupLocalVarTI' i (reverse ctx)
  where
    lookupLocalVarTI' :: Int -> [a] -> TI ann a
    lookupLocalVarTI' _ []     = throwError (pretty t <+> "not defined")
    lookupLocalVarTI' 0 (x:_)  = pure x
    lookupLocalVarTI' n (x:xs)
      | n < 0                  =
          error "internal error negative variable lookup"
      | otherwise              = lookupLocalVarTI' (n-1) xs

lookupDefTypeTI :: Text -- ^ name of expr var
                -> [TypeExpr] -- ^ type parameters to check
                -> [Expr] -- ^ expr parameters to check
                -> TI ann Type
lookupDefTypeTI t tyPars exprPars = view defCtx >>= lookupDefTypeTI'
  where
    lookupDefTypeTI' :: [Statement] -> TI ann Type
    lookupDefTypeTI' []                  =
         throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefTypeTI' (ExprDef{..}:stmts)
      | t == name                        =
          do parsKinds <- mapM inferType tyPars
             catchError (evalInTI $ betaeqTyCtx parsKinds tyParameterCtx)
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with type parameters"
                                       <+> pretty tyPars))
             parsTypes <- mapM inferTerm exprPars
             assert (all (null . fst) parsTypes)
                    "types expression parameters should have empty ctxts"
             catchError (evalInTI $ betaeqCtx (map snd parsTypes)
                                   (map (substPars 0 (reverse tyPars))
                                         exprParameterCtx))
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with expr parameters"
                                       <+> pretty exprPars))
             let (ctx',res) = fromJust ty
             pure ( map (substPars 0 (reverse tyPars)) ctx'
                  , substPars 0 (reverse tyPars) res)
      | otherwise                        =
          lookupDefTypeTI' stmts
    lookupDefTypeTI' (_:stmts)           =
          lookupDefTypeTI' stmts

lookupDefKindTI :: Text -- ^ name of type var
                -> [TypeExpr] -- ^ parameters to check
                -> TI ann Kind
lookupDefKindTI t pars = view defCtx >>= lookupDefKindTI'
  where
    lookupDefKindTI' :: [Statement] -> TI ann Kind
    lookupDefKindTI' []                  =
          throwError $ "Variable" <+> pretty t <+> "not defined"
    lookupDefKindTI' (TypeDef{..}:stmts)
      | t == name                        =
          do catchError (checkParKinds pars parameterCtx)
                        (throwError . (<> "\n while looking up variable "
                                       <+> pretty t
                                       <+> "with parameters"
                                       <+> pretty pars))
             pure (fromJust kind)
      | otherwise                        =
          lookupDefKindTI' stmts
    lookupDefKindTI' (_:stmts)           =
          lookupDefKindTI' stmts

checkParKinds :: [TypeExpr] -> TyCtx -> TI ann ()
checkParKinds []         []          =
  pure ()
checkParKinds (par:pars) (ctx':ctxs) =
  do kind <- local (ctx .~ []) $ inferType par
     evalInTI $ betaeqCtx ctx' kind
     checkParKinds pars (substParInParCtx 0 par ctxs)
checkParKinds _          _           =
  throwError "number of parameters doesn't match the expected one"

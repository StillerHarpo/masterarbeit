{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module TypeAction where

import           Lens.Micro.Platform

import           Data.Text.Prettyprint.Doc
import           Data.Text                      (Text)
import qualified Data.Text                 as T

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Arrow                  (second)

import           AbstractSyntaxTree
import           ShiftFreeVars
import           Subst

import           PrettyPrinter

-- | Evaluation monad for looking up global variables
type Eval ann = ExceptT (Doc ann) (Reader EvalCtx)

data EvalCtx = EvalCtx { numPars :: Int
                       , stmtCtx :: [Statement]}

emptyEvalCtx :: EvalCtx
emptyEvalCtx = EvalCtx { numPars = 0
                       , stmtCtx = []}

runEval :: Eval ann a -> EvalCtx -> Either (Doc ann) a
runEval eval = runReader (runExceptT eval)

typeAction :: TypeExpr
           -> [Expr]
           -> [Ctx]
           -> [TypeExpr]
           -> [TypeExpr]
           -> Eval ann Expr
typeAction (LocalTypeVar n _) terms _      _   _ = do
   l <- asks numPars
   pure $ terms !! (n + l)
typeAction (Parameter n _)    terms _      _   _ = pure $ terms !! n
typeAction (GlobalTypeVar n vars) terms gammas as' bs  = do
  tyExpr <- lookupDefTypeExpr n vars
  typeAction tyExpr terms gammas as' bs
typeAction (c :@ s)           terms gammas as' bs =
  flip (substExpr 0) s <$> typeAction c terms gammas as' bs
typeAction (Abstr _ c) terms gammas as' bs = typeAction c terms gammas as' bs
typeAction (In d)             terms gammas as' bs = do
  let idDeltas = map (map (shiftFreeVarsExpr 1 0) . idCtx . gamma1)
                     (strDefs d)
      fromRec = d { strDefs =
                      map (\strDef@StrDef{..} ->
                           strDef { a = substTypes 1 (zipWith abstrArgs
                                                               as'
                                                               gammas)
                                                       a })
                           (strDefs d) } -- R_a in paper
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
                                  idDeltas (map a (strDefs d))  [0..]
  pure$  applyExprArgs (Rec {..}  , idCtx (gamma d)) :@: LocalExprVar 0 ""
typeAction (Coin d)           terms gammas as' bs = do
  let idDeltas = map (map (shiftFreeVarsExpr 1 0) . idCtx . gamma1)
                     (strDefs d)
      toCorec = d { strDefs =
                      map (\strDef@StrDef{..} ->
                            strDef { a = substTypes 1 (zipWith abstrArgs
                                                               bs
                                                               gammas)
                                                       a})
                          (strDefs d)}
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
                                 idDeltas (map a (strDefs d)) [0..]
  pure $ applyExprArgs (Corec {..}  ,idCtx (gamma d)) :@: LocalExprVar 0 ""
typeAction _                  _     _      _   _  =
  pure $ LocalExprVar 0 ""

-- | splits up a chain of left associative applications to a expression
-- into a list of  arguments
getExprArgs :: Expr -> (Expr,[Expr])
getExprArgs (expr :@: arg) = second (++ [arg]) (getExprArgs expr)
getExprArgs arg            = (arg,[])

applyExprArgs :: (Expr,[Expr]) -> Expr
applyExprArgs (f,args) = foldl (:@:) f args

abstrArgs :: TypeExpr -> [TypeExpr] -> TypeExpr
abstrArgs = foldr Abstr

idCtx :: Ctx -> [Expr]
idCtx []  = []
idCtx [_] = [LocalExprVar 0 ""]
idCtx ctx = map (flip LocalExprVar "")
                [length ctx - 1, length ctx - 2 .. 0]

-- | splits up a chain of left associative applications to a type into a
-- list of  arguments
getTypeExprArgs :: TypeExpr -> (TypeExpr,[Expr])
getTypeExprArgs (expr :@ arg) = second (++ [arg]) (getTypeExprArgs expr)
getTypeExprArgs arg           = (arg,[])

applyTypeExprArgs :: (TypeExpr,[Expr]) -> TypeExpr
applyTypeExprArgs (f,args) = foldl (:@) f args

data InOrCoin = IsIn | IsCoin

-- | apply the context of a ductive to it
applyDuctiveCtx :: InOrCoin -> Ductive -> TypeExpr
applyDuctiveCtx IsIn   d@Ductive{..} =
  applyTypeExprArgs (In d, idCtx gamma)
applyDuctiveCtx IsCoin d@Ductive{..} =
  applyTypeExprArgs (Coin d, idCtx gamma)

lookupDefExpr :: Text -- ^ name of expr var
              -> [TypeExpr] -- ^ type parameters to check
              -> [Expr] -- ^ expr parameters to check
              -> Eval ann Expr
lookupDefExpr t tyPars exprPars = asks stmtCtx >>= lookupDefExpr'
  where
    lookupDefExpr' :: [Statement] -> Eval ann Expr
    lookupDefExpr' []         = throwError $ "Variable"
                                              <+> pretty t
                                              <> "not defined"
    lookupDefExpr' (ExprDef{..}:stmts)
      | t == name             = do
         assert (length tyPars == length tyParameterCtx)
                ("parameters to type variables have to be complete"
                  <> "\n while looking up" <+> pretty t
                  <> "\n with parameters" <+> pretty tyPars
                  <> "\n and parameter context"
                  <+> pretty tyParameterCtx)
         assert (length exprPars == length exprParameterCtx)
                ("parameters to type variables have to be complete"
                 <> "\n while looking up" <+> pretty t
                 <> "\n with parameters" <+> pretty exprPars
                 <> "\n and parameter context"
                 <+> pretty exprParameterCtx)
         pure $ substParsInExpr 0 (reverse tyPars)
              $ substExprs 0 (reverse exprPars) expr
      | otherwise             = lookupDefExpr' stmts
    lookupDefExpr' (_:stmts)  = lookupDefExpr' stmts

lookupDefTypeExpr :: Text -- ^ name of type var
                  -> [TypeExpr] -- ^ parameters to check
                  -> Eval ann TypeExpr
lookupDefTypeExpr t pars = asks stmtCtx >>= lookupDefTypeExpr'
  where
    lookupDefTypeExpr' :: [Statement] -> Eval ann TypeExpr
    lookupDefTypeExpr' []        = throwError $ "Variable" <+> pretty t
                                                 <+> "not defined"
    lookupDefTypeExpr' (TypeDef{..}:stmts)
      | t == name                = do
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
      | otherwise                = lookupDefTypeExpr' stmts
    lookupDefTypeExpr' (_:stmts) = lookupDefTypeExpr' stmts

assert :: MonadError e m => Bool -> e -> m ()
assert True  _   = pure ()
assert False msg = throwError msg

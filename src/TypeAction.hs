{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module TypeAction where

import           Data.Text.Prettyprint.Doc
import           Data.Text                 (Text)

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Arrow             (second)

import           AbstractSyntaxTree
import           ShiftFreeVars
import           Subst

import           PrettyPrinter             ()

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
           -> Int
           -> [Expr]
           -> [Ctx]
           -> [TypeExpr]
           -> [TypeExpr]
           -> Eval ann Expr
typeAction (LocalTypeVar i _ _) _  terms _      _   _ =
   pure $  terms !! i
typeAction Parameter{}          _  _     _      _   _ =
  error "Internatal error: parameter in type action"
typeAction (GlobalTypeVar n vars) j terms gammas as' bs  = do
  tyExpr <- lookupDefTypeExpr n vars
  typeAction tyExpr j terms gammas as' bs
typeAction (c :@ s)               j terms gammas as' bs =
  -- TODO Make sure j is right in all cases
  -- maybe variable in s should be marked until type action
  -- is finished (didn't work for one test)
  flip (substExpr j) s
  <$> typeAction c (j+1) terms gammas as' bs
typeAction (Abstr _ c)            j terms gammas as' bs =
  typeAction c (j-1) terms gammas as' bs
typeAction Ductive{..}            _ terms gammas as bs =
  let OpenDuctive{..} = openDuctive
      abstrAs = zipWith abstrArgs as gammas -- R_a in paper
      abstrBs = zipWith abstrArgs bs gammas -- R_b in paper
      strDefsAs = map (\strDef@StrDef{..} ->
                                 strDef { a = substTypes 1 abstrAs a })
                       strDefs
      strDefsBs = map (\strDef@StrDef{..} ->
                                 strDef { a = substTypes 1 abstrBs a })
                       strDefs
      idDeltas = map (map (shiftFreeVarsExpr 1 0) . idCtx . gamma1)
                            strDefs
  in case inOrCoin of
       IsIn -> do
         let parameters = map (substTypes 0 abstrBs) parametersTyExpr
             ductive = OpenDuctive{..} { strDefs =  strDefsBs }
             openDuctive = OpenDuctive{..} { strDefs =  strDefsBs }
             motive = Ductive{..}
               { parametersTyExpr = map (substTypes 0 abstrBs) parametersTyExpr }
         matches <- sequence $ zipWith3 (\idDelta dk num -> do
           recEval <- typeAction dk
                                 1
                                (LocalExprVar 0 False "" : terms)
                                (gamma : gammas)
                                (motive : as)
                                (motive : bs )
           pure $ applyExprArgs (Structor{..}, idDelta)
                                 :@: recEval) -- g_k in paper
                                         idDeltas
                                         (map (substPars 0 (reverse
                                                            $ map (shiftFreeTypeVars 1 0)
                                                                   parametersTyExpr) . a)
                                               strDefs)
                                         [0..]
         let parameters = map (substTypes 0 abstrAs) parametersTyExpr
             ductive = OpenDuctive{..} { strDefs =  strDefsAs }
         pure $ applyExprArgs ( Iter {..}
                               , map (shiftFreeVarsExpr 1 0) $ idCtx gamma)
                :@: LocalExprVar 0 False ""
       IsCoin -> do
         let parameters = map (substTypes 0 abstrAs) parametersTyExpr
             ductive = OpenDuctive{..} { strDefs =  strDefsAs }
             openDuctive = OpenDuctive{..} { strDefs =  strDefsAs }
             motive = Ductive{..}
               { parametersTyExpr = map (substTypes 0 abstrAs) parametersTyExpr }
         matches <- sequence $ zipWith3 (\idDelta dk num -> do
           recEval <- typeAction dk
                                 1
                                 (LocalExprVar 0 False "" : terms )
                                 (gamma : gammas )
                                 (motive : as)
                                 (motive : bs )
           pure $ substExpr 0 (applyExprArgs ( Structor{..} , idDelta)
                               :@: LocalExprVar 0 False "")
                              recEval)
                                        idDeltas
                                        (map (substPars 0 (reverse
                                                           $ map (shiftFreeTypeVars 1 0)
                                                                  parametersTyExpr) . a)
                                              strDefs)
                                        [0..]
         let ductive = OpenDuctive{..} { strDefs =  strDefsBs }
             parameters = map (substTypes 0 abstrBs) parametersTyExpr
         pure $ applyExprArgs ( Iter {..}
                              , map (shiftFreeVarsExpr 1 0) $ idCtx gamma)
                :@: LocalExprVar 0 False ""
typeAction _                  _ _     _      _   _  =
  pure $ LocalExprVar 0 False ""

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
idCtx [_] = [LocalExprVar 0 False ""]
idCtx ctx = map (\i -> LocalExprVar i False "")
                [length ctx - 1, length ctx - 2 .. 0]

-- | splits up a chain of left associative applications to a type into a
-- list of  arguments
getTypeExprArgs :: TypeExpr -> (TypeExpr,[Expr])
getTypeExprArgs (expr :@ arg) = second (++ [arg]) (getTypeExprArgs expr)
getTypeExprArgs arg           = (arg,[])

applyTypeExprArgs :: (TypeExpr,[Expr]) -> TypeExpr
applyTypeExprArgs (f,args) = foldl (:@) f args

lookupDefExpr :: Text -- ^ name of expr var
              -> [TypeExpr] -- ^ type parameters to check
              -> [Expr] -- ^ expr parameters to check
              -> Eval ann Expr
lookupDefExpr t tyPars exprPars = asks stmtCtx >>= lookupDefExpr'
  where
    lookupDefExpr' :: [Statement] -> Eval ann Expr
    lookupDefExpr' []         = throwError $ "Variable"
                                              <+> pretty t
                                              <+> "not defined"
    lookupDefExpr' (ExprDef{..}:stmts)
      | t == name             = do
         -- TODO should be unnecessary because it's already type checked
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
lookupDefTypeExpr t parametersTyExpr = asks stmtCtx >>= lookupDefTypeExpr'
  where
    lookupDefTypeExpr' :: [Statement] -> Eval ann TypeExpr
    lookupDefTypeExpr' []        = throwError $ "Variable" <+> pretty t
                                                 <+> "not defined"
    lookupDefTypeExpr' (TypeDef openDuctive@OpenDuctive{..}:stmts)
      | t == nameDuc             = pure Ductive{..}
      | otherwise                = lookupDefTypeExpr' stmts
    lookupDefTypeExpr' (_:stmts) = lookupDefTypeExpr' stmts

assert :: MonadError e m => Bool -> e -> m ()
assert True  _   = pure ()
assert False msg = throwError msg

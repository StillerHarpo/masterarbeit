module Mark where

import AbstractSyntaxTree

markFuns :: OverFuns
markFuns = (overFuns markFuns) { fTyExpr = markInTyExpr
                               , fExpr = markInExpr}

markInTyExpr :: TypeExpr -> TypeExpr
markInTyExpr (LocalTypeVar i _ n) = LocalTypeVar i True n
markInTyExpr (Parameter i _ n)    = Parameter i True n
markInTyExpr e                    = overTypeExpr markFuns e

markInExpr :: Expr -> Expr
markInExpr (LocalExprVar i _ n) = LocalExprVar i True n
markInExpr e                    = overExpr markFuns e

markInDuc :: OpenDuctive -> OpenDuctive
markInDuc = overOpenDuctive markFuns

markInStrDef :: StrDef -> StrDef
markInStrDef = overStrDef markFuns

unmarkFuns :: OverFuns
unmarkFuns = (overFuns unmarkFuns) { fTyExpr = unmarkInTyExpr
                                   , fExpr = unmarkInExpr}

unmarkInTyExpr :: TypeExpr -> TypeExpr
unmarkInTyExpr (LocalTypeVar i _ n) = LocalTypeVar i False n
unmarkInTyExpr (Parameter i _ n)    = Parameter i False n
unmarkInTyExpr e                    = overTypeExpr unmarkFuns e

unmarkInExpr :: Expr -> Expr
unmarkInExpr (LocalExprVar i _ n) = LocalExprVar i False n
unmarkInExpr e                    = overExpr unmarkFuns e

unmarkInDuc :: OpenDuctive -> OpenDuctive
unmarkInDuc = overOpenDuctive unmarkFuns

unmarkInStrDef :: StrDef -> StrDef
unmarkInStrDef = overStrDef unmarkFuns

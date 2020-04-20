module Main where

import Prelude

import Algebra.BoundVars (boundVarsAlg)
import Algebra.FreeVars (freeVarsAlg)
import Algebra.Show (showAlg)
import EADT (EADT)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Matryoshka (cata)
import Term.App (App, app)
import Term.Lam (Lam, lam)
import Term.Var (Var, var)
import Type.Eval (class Eval, kind TypeExpr) as TF
import Type.Eval.ValueOf (from) as TF
import Type.Row (type (+))

-- aka typefamily for UTLC
foreign import data UT :: Type -> TF.TypeExpr

instance evalUTString :: TF.Eval (UT String) String

type BASE tf = Var + App + (Lam tf) + ()
type UTLC = EADT (BASE UT)

freeVars :: UTLC -> Array String
freeVars = cata freeVarsAlg

boundVars :: UTLC -> Array String
boundVars = cata boundVarsAlg

showUTLC :: UTLC -> String
showUTLC = cata showAlg

ulam :: String -> UTLC -> UTLC
ulam t b = lam (TF.from t) b

main :: Effect Unit
main = do
  let const = ulam "x" (ulam "y" (var "x")) -- const
  let omega = app (ulam "x" (app (var "x") (var "x"))) (ulam "x" (app (var "x") (var "x"))) -- omega

  log $ showUTLC const
  log $ showUTLC omega

  logShow $ freeVars const
  logShow $ freeVars omega
  
  logShow $ boundVars const
  logShow $ boundVars omega

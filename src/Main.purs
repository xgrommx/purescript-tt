module Main where

import Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import EADT (EADT)
import Effect (Effect)
import Effect.Class.Console (log)
import Matryoshka (Algebra, cata)
import Term.App (App, AppF(..), app)
import Term.Lam (Lam, LamF(..), lam)
import Term.Var (Var, VarF(..), var)
import Type.Eval (class Eval, kind TypeExpr) as TF
import Type.Eval.ValueOf (from, to) as TF
import Type.Row (type (+))

-- aka typefamily for UTLC
foreign import data UT :: Type -> TF.TypeExpr

instance evalUTString :: TF.Eval (UT String) String

type BASE tf = Var + App + (Lam tf) + ()
type UTLC = EADT (BASE UT)

fvAlg :: Algebra (VariantF (BASE UT)) (Array String)
fvAlg = VF.match
  { var: \(VarF t) -> [t]
  , lam: \(LamF t b) -> A.filter (\t' -> t' /= TF.to t) b
  , app: \(AppF l a) -> l <> a
  }

bndAlg :: Algebra (VariantF (BASE UT)) (Array String)
bndAlg = VF.match
  { var: \(VarF t) -> []
  , lam: \(LamF t b) -> TF.to t : b
  , app: \(AppF l a) -> l <> a
  }

showAlg :: Algebra (VariantF (BASE UT)) String
showAlg = VF.match
  { var: \(VarF t) -> t
  , lam: \(LamF t b) -> "\\" <> TF.to t <> "." <> b
  , app: \(AppF l a) -> "(" <> l <> ")(" <> a <> ")"
  }

freeVars :: UTLC -> Array String
freeVars = cata fvAlg

boundVars :: UTLC -> Array String
boundVars = cata bndAlg

showUTLC :: UTLC -> String
showUTLC = cata showAlg

ulam :: String -> UTLC -> UTLC
ulam t b = lam (TF.from t) b

main :: Effect Unit
main = do
  log $ showUTLC $ ulam "x" (ulam "y" (var "x")) -- const
  log $ showUTLC $ app (ulam "x" (app (var "x") (var "x"))) (ulam "x" (app (var "x") (var "x"))) -- omega

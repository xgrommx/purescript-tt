module Term.Var where

import Prelude

import Algebra.BoundVars (class BoundVarsAlg, boundVarsAlg)
import Algebra.FreeVars (class FreeVarsAlg, freeVarsAlg)
import Algebra.Show (class ShowAlg)
import Data.Functor.Mu (Mu)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Lens (Prism', prism', re, review)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import EADT (EADT, _Mu, _VariantF, sym)
import Type.Equality (class TypeEquals)
import Type.Equality as TE

-- base functor
newtype VarF a = VarF String

-- row type
type Var r = (var ∷ VF.FProxy VarF | r)

-- typelevel label
_var = sym ∷ _ "var"

-- smart constructor
var ∷ forall r. String -> EADT (Var r)
var = review _VarF

-- eqVar :: EADT (Var + ()) -> EADT (Var + ()) -> Boolean
-- eqVar (In v1) (In v2)
--   | Just (VarF n1) <- VF.prj _var v1
--   , Just (VarF n2) <- VF.prj _var v2 = n1 == n2
--   | otherwise = false

  
-- eqVar (In v1) (In v2) = case VF.prj _var v1, VF.prj _var v2 of
--   Just (VarF n1), Just (VarF n2) -> n1 == n2
--   _, _ -> false

derive instance functorVarF ∷ Functor VarF

-- algebras

instance showAlgVarF :: ShowAlg VarF where
  showAlg (VarF a) = a

instance freeVarsAlgVarF :: FreeVarsAlg VarF where
  freeVarsAlg (VarF a) = [a]

instance boundVarsAlgVarF :: BoundVarsAlg VarF where
  boundVarsAlg (VarF a) = []

-- instance eqVarF :: Eq a => Eq (VarF a) where
--   eq e1 e2 = genericEq e1 e2

-- instance eq1VarF :: Eq1 VarF where
--   eq1 e1 e2 = eq e1 e2

-- classy prism

class AsVarF s a | s -> a where
  _VarF ∷ Prism' s String

instance asVarFVarF ∷ AsVarF (VarF a) a where
  _VarF = prism' VarF (\(VarF a) -> Just a)

else instance asVarFVariant :: (Functor f, AsVarF (f a) a, TypeEquals (VariantF (var :: VF.FProxy f | tail) a) (VariantF row a)) => AsVarF (VariantF row a) a where
  _VarF = dimap TE.from TE.to <<< _VariantF _var <<< _VarF

else instance asVarFFMu :: (Functor f, AsVarF (f (Mu f)) a) => AsVarF (Mu f) a where
  _VarF = re _Mu <<< _VarF
module Term.Lam where

import Prelude

import Algebra.BoundVars (class BoundVarsAlg)
import Algebra.FreeVars (class FreeVarsAlg, freeVarsAlg)
import Algebra.Show (class ShowAlg)
import Data.Array ((:))
import Data.Array as A
import Data.Functor.Mu (Mu)
import Data.Functor.Variant (VariantF)
import Data.Lens (Prism', prism', re, review)
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Tuple (Tuple(..), uncurry)
import Data.Variant.Internal as VF
import EADT (EADT, _Mu, _VariantF, sym)
import Type.Equality (class TypeEquals)
import Type.Equality as TE
import Type.Eval (class Eval)
import Type.Eval.ValueOf (ValueOf)
import Type.Eval.ValueOf as TF
import Type.Row (type (+))

-- base functor
data LamF tf a = LamF (ValueOf (tf String)) a

-- row type
type Lam tf r = (lam ∷ VF.FProxy (LamF tf) | r)

-- typelevel label
_lam = sym ∷ _ "lam"

-- smart constructor
lam ∷ forall r tf. ValueOf (tf String) -> EADT (Lam tf + r) -> EADT (Lam tf + r)
lam x y = review _LamF (Tuple x y)

derive instance functorLamF ∷ Functor (LamF tf)

-- algebras

instance showAlgLamF :: Eval (tf String) String => ShowAlg (LamF tf) where
  showAlg (LamF t b) = "\\" <> TF.to t <> "." <> b

instance freeVarsAlgLamF :: Eval (tf String) String => FreeVarsAlg (LamF tf) where
  freeVarsAlg (LamF t b) = A.filter (\t' -> t' /= TF.to t) b

instance boundVarsAlgLamF :: Eval (tf String) String => BoundVarsAlg (LamF tf) where
  boundVarsAlg (LamF t b) = TF.to t : b

-- classy prism

class AsLamF tf s a | tf s -> a where
  _LamF ∷ Prism' s (Tuple (ValueOf (tf String)) a)

instance asLamFLamF ∷ AsLamF tf (LamF tf a) a where
  _LamF = prism' (uncurry LamF) (\(LamF a b) -> Just (Tuple a b))

else instance asLamFVariant :: (Functor f, AsLamF tf (f a) a, TypeEquals (VariantF (lam :: VF.FProxy f | tail) a) (VariantF row a)) => AsLamF tf (VariantF row a) a where
  _LamF = dimap TE.from TE.to <<< _VariantF _lam <<< _LamF

else instance asLamFMu :: (Functor f, AsLamF tf (f (Mu f)) a) => AsLamF tf (Mu f) a where
  _LamF = re _Mu <<< _LamF
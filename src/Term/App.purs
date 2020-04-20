module Term.App where

import Prelude

import Algebra.BoundVars (class BoundVarsAlg)
import Algebra.FreeVars (class FreeVarsAlg)
import Algebra.Show (class ShowAlg)
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
import Type.Row (type (+))

-- base functor
data AppF a = AppF a a

-- row type
type App r = (app ∷ VF.FProxy AppF | r)

-- typelevel label
_app = sym ∷ _ "app"

-- smart constructor
app ∷ forall r. EADT (App + r) -> EADT (App + r) -> EADT (App + r)
app x y = review _AppF (Tuple x y)

derive instance functorAppF ∷ Functor AppF

-- algebras

instance showAlgAppF :: ShowAlg AppF where
  showAlg (AppF f a) = "(" <> f <> ")(" <> a <> ")"

instance freeVarsAlgAppF :: FreeVarsAlg AppF where
  freeVarsAlg (AppF f a) = f <> a

instance boundVarsAlgAppF :: BoundVarsAlg AppF where
  boundVarsAlg (AppF f a) = f <> a

-- classy prism

class AsAppF s a | s -> a where
  _AppF ∷ Prism' s (Tuple a a)

instance asAppFAppF ∷ AsAppF (AppF a) a where
  _AppF = prism' (uncurry AppF) (\(AppF a b) -> Just (Tuple a b))

else instance asAppFVariant :: (Functor f, AsAppF (f a) a, TypeEquals (VariantF (app :: VF.FProxy f | tail) a) (VariantF row a)) => AsAppF (VariantF row a) a where
  _AppF = dimap TE.from TE.to <<< _VariantF _app <<< _AppF

else instance asAppFMu :: (Functor f, AsAppF (f (Mu f)) a) => AsAppF (Mu f) a where
  _AppF = re _Mu <<< _AppF
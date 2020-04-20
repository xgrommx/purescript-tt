module EADT where

import Prelude

import Data.Functor.Mu (Mu, roll, unroll)
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Lens (Iso, Prism', iso, prism')
import Data.Symbol (class IsSymbol)
import Prim.Row as R

sym :: forall s. IsSymbol s => VF.SProxy s
sym = VF.SProxy

type EADT t = Mu (VariantF t)

_Mu ∷ forall f g. Iso (f (Mu f)) (g (Mu g)) (Mu f) (Mu g)
_Mu = iso roll unroll

_VariantF
  :: forall l f v a
  . IsSymbol l
  => Functor f
  => R.Cons l (VF.FProxy f) _ v
  => VF.SProxy l
  -> Prism' (VF.VariantF v a) (f a)
_VariantF label = prism' (VF.inj label) (VF.prj label)
module Algebra.BoundVars where

import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Symbol (class IsSymbol)
import EADT (sym)
import Matryoshka (Algebra)
import Prim.Row as R
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))

class BoundVarsAlg h where
  boundVarsAlg :: Algebra h (Array String)

class BoundVarsAlgVFRL (rl :: RL.RowList) (row :: # Type) | rl -> row where
  boundVarsAlgVFRL :: RLProxy rl -> Algebra (VariantF row) (Array String)

instance boundVarsAlgVFRLNil :: BoundVarsAlgVFRL RL.Nil () where
  boundVarsAlgVFRL _ = VF.case_

instance boundVarsAlgVFRLCons :: (IsSymbol l, BoundVarsAlg h, BoundVarsAlgVFRL rl r, R.Cons l (VF.FProxy h) r r') => BoundVarsAlgVFRL (RL.Cons l (VF.FProxy h) rl) r' where
  boundVarsAlgVFRL _ = VF.on l boundVarsAlg (boundVarsAlgVFRL (RLProxy :: RLProxy rl)) where
    l = sym :: _ l

instance boundVarsAlgVariantF :: (RL.RowToList row rl, BoundVarsAlgVFRL rl row) => BoundVarsAlg (VariantF row) where
  boundVarsAlg = boundVarsAlgVFRL (RLProxy :: RLProxy rl)
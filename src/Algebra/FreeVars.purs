module Algebra.FreeVars where

import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Symbol (class IsSymbol)
import EADT (sym)
import Matryoshka (Algebra)
import Prim.Row as R
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))

class FreeVarsAlg h where
  freeVarsAlg :: Algebra h (Array String)

class FreeAlgVarsVFRL (rl :: RL.RowList) (row :: # Type) | rl -> row where
  freeVarsAlgVFRL :: RLProxy rl -> Algebra (VariantF row) (Array String)

instance freeVarsAlgVFRLNil :: FreeAlgVarsVFRL RL.Nil () where
  freeVarsAlgVFRL _ = VF.case_

instance freeVarsAlgVFRLCons :: (IsSymbol l, FreeVarsAlg h, FreeAlgVarsVFRL rl r, R.Cons l (VF.FProxy h) r r') => FreeAlgVarsVFRL (RL.Cons l (VF.FProxy h) rl) r' where
  freeVarsAlgVFRL _ = VF.on l freeVarsAlg (freeVarsAlgVFRL (RLProxy :: RLProxy rl)) where
    l = sym :: _ l

instance freeVarsAlgVariantF :: (RL.RowToList row rl, FreeAlgVarsVFRL rl row) => FreeVarsAlg (VariantF row) where
  freeVarsAlg = freeVarsAlgVFRL (RLProxy :: RLProxy rl)
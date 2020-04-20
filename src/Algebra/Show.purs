module Algebra.Show where

import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VF
import Data.Symbol (class IsSymbol)
import EADT (sym)
import Matryoshka (Algebra)
import Prim.Row as R
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))

class ShowAlg h where
  showAlg :: Algebra h String

class ShowAlgVFRL (rl :: RL.RowList) (row :: # Type) | rl -> row where
  showAlgVFRL :: RLProxy rl -> Algebra (VariantF row) String

instance showAlgVFRLNil :: ShowAlgVFRL RL.Nil () where
  showAlgVFRL _ = VF.case_

instance showAlgVFRLCons :: (IsSymbol l, ShowAlg h, ShowAlgVFRL rl r, R.Cons l (VF.FProxy h) r r') => ShowAlgVFRL (RL.Cons l (VF.FProxy h) rl) r' where
  showAlgVFRL _ = VF.on l showAlg (showAlgVFRL (RLProxy :: RLProxy rl)) where
    l = sym :: _ l

instance showAlgVariantF :: (RL.RowToList row rl, ShowAlgVFRL rl row) => ShowAlg (VariantF row) where
  showAlg = showAlgVFRL (RLProxy :: RLProxy rl)
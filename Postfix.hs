module Postfix (
  postfix,
  dup,
  tuck,
  over,
  swap,
  rot,
  drp,
  nip
) where

import Linear as L
import Operators
import Data.Monoid
import Prelude hiding (take)
import Data.Foldable as F

-- Postfix evaluator for a sequence of operators
postfix ops = if null result then Nothing else Just (L.head result) where
  result = applyOp (F.foldl mappend mempty ops) []

--helper functions
dup' (a:as) = a:a:as
dup' as = as
tuck' (a:a':as) = a:a':a:as
tuck' as = as
over' (a:a':as) = a':a:a':as
over' as = as
rot' (a:a':a'':as) = a'':a:a':as
rot' as = as
swap' = drp' . tuck'
drp' = Prelude.drop 1
nip' = drp' . swap' 
-- Operators for basic stack manipulation.
dup  = OP { eats=1, leaves=2, applyOp=dup',  name="dup"}
tuck = OP { eats=2, leaves=3, applyOp=tuck', name="tuck"}
over = OP { eats=2, leaves=3, applyOp=over', name="over"}
swap = OP { eats=2, leaves=2, applyOp=swap', name="swap"}
rot  = OP { eats=3, leaves=3, applyOp=rot',  name="rot"}
drp  = OP { eats=1, leaves=0, applyOp=drp',  name="drop"}
nip  = OP { eats=2, leaves=1, applyOp=nip',  name="nip"}

module LinArr (
  LinArr
) where

import GeneticOperators
import Data.Array

data LinArr a = Arr (Array Int a)
              | Slice Int Int (LinArr a)

size = snd . bounds

instance Linear LinArr where
  first (Arr ar) = ar ! 0
  first (Slice start _ ar) = ar ! start
  rest start (Arr ar) = Slice (min 1 (size ar)) ar
  rest (Slice start end ar) = Slice (min end (start+1)) end ar
  take end ar@(Arr _) = Slice 0 end ar
  take end (Slice s e ar) = Slice s (min e (e+end)) ar
  drop d (Arr ar) = Slice d (size  ar) ar
  drop d (Slice start end ar) = Slice (min end (start+1)) end ar
  count (Arr ar) = size ar
  count (Slice s e ar) = e - s
  isEmpty (Arr ar) = size ar == 0
  isEmpty (Slice s e _) = e - s <= 0
  empty = Arr $ listArray (0, 0) []
  index i (Arr ar) = ar ! i
  index i (Slice s e ar) = if (i+s) >= e then error "out of bounds" else ar ! (i+s)
  update i a (Arr ar) = Arr $ ar // [(i, a)]
  update n a' (Slice s e ar) = if n >= 0 && (n+s) < e then Slice s e $ update (s+n) ar else error "index out of bounds"

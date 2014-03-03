module Structures (
  Linear(..)
) where

import qualified Data.List as L 
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
import qualified Data.Vector as V
import qualified Data.Array.Diff as A


-- finite linear structures
-- minimal complete definition: count * (cut + break)
class (Tr.Traversable l, Monoid l, Monad l) => Linear l where
  count :: l a -> Int
  index :: l a -> Int -> a
  index s i = let (_, a, _) = break i s in a
  update :: Int -> a -> l a -> l a
  update i a s = let (h, _, t) = break i s in h `mappend` return a `mappend` t
  slice :: Int -> Int -> s a -> s a
  slice b e s = let ((_, sliced), _) = fst $ cut (e-b) $ snd $ cut b s
  break :: Int -> s a -> (s a, a, s a)
  break n s = let (h, t) = cut n s in (h, index t 0, slice 1 (count t) t)
  cut :: Int -> s a -> (s a, s a)
  cut n s = let (h, a, t) = break n s in (h, return a `mappend` t)

instance Linear [] where
  first = head
  rest = L.tail
  take = L.take
  drop = L.drop
  count = length
  isEmpty = null
  empty = []
  index = (!!)
  update _ _ [] = []
  update 0 a (_:as) = a:as
  update n a (a':as) = a':update (n-1) a as
  cons = (:)

instance Linear S.Seq where
  first s = s `S.index` 0
  rest = S.drop 1
  take = S.take
  drop = S.drop
  count = S.length
  isEmpty = S.null
  empty = S.empty
  index = S.index
  update = S.update
  cons = (S.<|)

instance Linear V.Vector where
  first = V.head
  rest = V.drop 1
  take = V.take
  drop = V.drop
  count = V.length
  isEmpty = V.null
  empty = V.empty
  index = (V.!)
  update i a v = v V.// [(i, a)]
  cons = V.cons

module Pairable (
  Pairable(..)
) where

import Randomly
import Data.Sequence as S
import Data.Foldable as F
import Data.Traversable as Tr
import Data.Vector as V

class (Functor p) => Pairable p where
  pairup :: p a -> p (a,a)
  pairup s = onpairs id s
  unpair :: p (a,a) -> p a
  onpairs :: ((a, a) -> b) -> p a -> p b
  onpairs f s = fmap f $ pairup s

instance Pairable [] where
  pairup [] = []
  pairup (_:[]) = []
  pairup (x:y:xs) = (x,y):pairup xs
  unpair [] = []
  unpair ((a,b):as) = a:b:unpair as

instance Pairable Seq where
  pairup s = S.zip (S.take half s) (S.drop half s) where
    half = S.length s `div` 2
  unpair s = fmap fst s >< fmap snd s

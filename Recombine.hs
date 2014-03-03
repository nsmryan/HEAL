module Recombine(
  mutation,
  crossover,
  rotation,
  Pairable(..),
  cross
) where

import EAMonad
import Linear
import Pairable
import Randomly
import Control.Monad(replicateM)
import Data.Monoid
import qualified Data.Traversable as Tr
import Control.Applicative as A
import qualified Data.Foldable as F


mutation :: (Randomizable a, Linear f, Linear g) =>
  Double -> f (g a) -> EAMonad (f (g a)) e
--complex efficient mutation
mutation pm pop = let 
  size = count pop
  indlen = count $ index pop 0
  ms = ceiling $ (pm *) $ fromIntegral (size*indlen)
  mut pop i = do
      let ind = index pop i
      idx <- nextInt indlen
      val <- generateFrom $ index ind idx
      return $ update i (update idx val ind) pop in do
    muts <- replicateM ms (nextInt size)
    F.foldlM mut pop muts
--simple inefficient mutation
--mutation pm pop = Tr.mapM (Tr.mapM (mightM pm generateFrom)) pop

crossover :: (Randomizable (l (f c)), Pairable l, Linear l, Linear f) =>
  Int -> Double -> l (f c) -> EAMonad (l (f c)) e
crossover n pc pop = do
  pop' <- generateFrom pop
  crossed <- Tr.mapM (mightM pc (cross n)) $ pairup pop'
  return $ unpair crossed
cross 0 pair = return pair
cross n (s, s') = do
  cp <- generateFrom $ count s
  cross (n-1) $ cross' cp s s'
cross' n s s' = (h <|> t', h' <|> t) where
  (h, t) = cut n s
  (h', t') = cut n s'

rotation :: (Tr.Traversable f, Linear r) => Int -> Double -> f (r a) -> EAMonad (f (r a)) e
rotation i pr pop = Tr.mapM (mightM pr (rotate i)) pop
rotate i ind = do
  idx <- generateFrom $ count ind 
  return $ rotate' ind $ idx `multipleOf` i
rotate' s n = uncurry (flip (<|>)) $ cut n s
multipleOf n i = i * (n `div` i)

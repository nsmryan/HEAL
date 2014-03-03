{-# LANGUAGE BangPatterns #-}
module Selection (
  roulette,
  tournament,
  elitism,
  best,
  bestInd,
  bestFit,
  Min(..),
) where

import EAMonad
import Randomly(nextDouble, nextInt, test, choose, chosen)
import Pairable
import Linear as L
import Control.Monad
import Data.Function
import Data.Foldable as F
import Data.Traversable as Tr
import Control.Applicative as A


newtype Min = Min { unMin :: Double } deriving Eq

instance Show Min where
  show (Min d) = show d
instance Ord Min where
  compare (Min d) (Min d') 
    | d == d' = EQ
    | d >= d' = LT
    | otherwise = GT

-- Roulette Wheel Selection.
roulette :: (Linear p) => p (a, Double) -> EAMonad (p a) e
roulette pop = let sumfit = F.sum $ fmap snd pop in do
  ds <- Tr.forM pop $ const (nextDouble sumfit)
  return $ fmap (select pop) ds
select v !d | isEmpty v = error $ "No selection, fitness remaining: " ++ show d
            | remaining <= 0.00001 = i
            | otherwise = select (L.tail v) remaining where
              (i, f) = L.head v
              remaining = d - f

tournament :: (Pairable p, Linear p, Ord b) => p (a, b) -> EAMonad (p a) e
tournament pop = do
  p <- chosen pop
  p' <- chosen pop
  Tr.mapM compete $ pairup $ p <|> p'
compete ((a, f), (a', f')) = do
  b <- test 0.75
  return $ if (f >= f') == b then a else a'

-- Best individual.
best :: (Linear p, Ord b) => p (a, b) -> (a, b)
best pop = F.maximumBy (\(_, a) (_, a') -> compare a a') pop
bestInd :: (Linear p, Ord b) => p (a, b) -> a
bestInd pop = fst $ best pop
bestFit :: (Linear p, Ord b) => p (a, b) -> b
bestFit pop = snd $ best pop

-- Elitism.
elitism :: (Linear p, Ord b) =>
  (p (a, b) -> EAMonad (p (a, b)) e) -> -- generation
  p (a, b) ->                           -- population
  EAMonad (p (a, b)) e
elitism gen pop = do
  pop' <- gen pop
  return $ update 0 (best pop) pop'

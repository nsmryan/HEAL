{-# LANGUAGE BangPatterns #-}
module Randomly (
  test,
  might,
  mightM,
  nextDouble,
  nextInt,
  nextBool,
  selectFrom,
  Randomizable(..),
  pick,
  choose,
  chosen
) where

import EAMonad
import Linear as L
import System.Random.Mersenne.Pure64
import Control.Monad.Mersenne.Random
import Prelude as P
import Data.Sequence as S
import Data.Traversable as Tr

class Randomizable r where
  generateFrom :: r -> EAMonad r e

instance Randomizable [a] where
  generateFrom as = shuffle as

instance Randomizable (Seq a) where
  generateFrom as = seqShuffle as

instance Randomizable Bool where
  generateFrom b = nextBool

instance Randomizable Int where
  generateFrom = nextInt

instance Randomizable Double where
  generateFrom = nextDouble 

nextDouble :: Double -> EAMonad Double e
nextDouble d = do
  x <- randomly getDouble 
  return $! x*d

nextInt :: Int -> EAMonad Int e
nextInt d = do
  x <- randomly getInt 
  return $! x `mod` d

nextBool :: EAMonad Bool e
nextBool = randomly getBool

test :: Double -> EAMonad Bool e
test p = do  
  x <- nextDouble 1 
  return $! p > x

might p f a = do
  b <- test p
  return $ if b then f a else a

mightM p f a = do
  b <- test p
  if b then f a else return $ a 

selectFrom :: [a] -> EAMonad a e
selectFrom (a:as) = fairSelect 2.0 a as where
  fairSelect _ a [] = return a
  fairSelect n a (a':as) = do
    b <- test (1.0/n)
    let n' = n+1.0
    if b then fairSelect n' a' as else fairSelect n' a as

shuffle :: [a] -> EAMonad [a] e
shuffle [] = return []
shuffle as = do 
  n <- nextInt $ P.length as
  remaining <- shuffle $ dropNth as n
  return $ (as !! n):remaining

dropNth :: [a] -> Int -> [a]
dropNth [] _ = []
dropNth (_:xs) 0 = xs
dropNth (x:xs) n = x:dropNth xs (n-1)

seqShuffle :: Seq a -> EAMonad (Seq a) e
seqShuffle s | S.null s = return S.empty
             | otherwise = do
              n <- nextInt $ S.length s
              remaining <- seqShuffle $ dropNthSeq s n
              return $ (s `S.index` n) <| remaining

dropNthSeq :: Seq a -> Int -> Seq a
dropNthSeq seq n = let (h, t) = S.splitAt n seq in h >< S.drop 1 t

pick :: (Linear l) => l a -> EAMonad a e
pick l = nextInt (count l) >>= return . L.index l
  
choose :: (Randomizable (l a), Linear l) => Int -> l a -> EAMonad (l a) e
choose n l = do
  l' <- generateFrom l
  return $ L.take n l'

chosen :: (Linear l) => l a -> EAMonad (l a) e
chosen l = Tr.mapM (const (pick l)) l

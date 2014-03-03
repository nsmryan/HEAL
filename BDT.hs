{-# LANGUAGE DeriveTraversable, DeriveFoldable, DeriveFunctor #-}
module BDT (
  BDT(..),
  decided,
  decide,
  evaltree,
  evalcases,
  treesize,
  treedepth
) where

import Operators
import Data.List(foldl', elem)
import Data.Foldable as F
import Data.Traversable

data BDT a = Leaf a | Node a (BDT a) (BDT a) deriving (Show, Eq, Functor, Foldable, Traversable)

decided k = OP {eats=0, leaves=1, applyOp=wrap0 (Leaf k), name=show k}
decide k = OP {eats=2, leaves=1, applyOp=wrap2 (Node k), name=show k}

evaltree _ (Leaf a) = a
evaltree attrs (Node a l r) = evaltree attrs $ if a `Prelude.elem` attrs then l else r

treesize (Leaf _) = 1
treesize (Node _ l r) = 1 + treesize l + treesize r
--treesize tree = F.foldl' (+) 0 $ fmap (const 1) tree

treedepth (Leaf _) = 1
treedepth (Node _ l r) = 1 + max (treedepth l) (treedepth r)
--treedepth tree = F.foldl' (\a b -> 1 + max a b) 0 $ fmap (const 1) tree

evalcases testcases tree = Prelude.sum $ map errorOnCase testcases where
  errorOnCase (cas, expected) = fromIntegral $ fromEnum $ evaltree cas tree == expected

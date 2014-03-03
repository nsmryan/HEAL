{-# LANGUAGE BangPatterns #-}
module Operators (
  OP(..),
  wrap0,
  wrap1,
  wrap2,
  wrap3,
  treeOP,
  treeConst
) where

import Data.Monoid
import Data.Tree as T
import qualified Data.Sequence as S

data OP a = OP { eats::Int, leaves::Int, applyOp::[a] -> [a], name::String }

instance Show (OP a) where
  show (OP _ _ _ name) = name

instance Eq (OP a) where
  (OP _ _  _ name) == (OP _ _ _ name') = name == name'

instance Monoid (OP a) where
  mempty = OP 0 0 id "id"
  (OP eat leave ap nam) `mappend` (OP eat' leave' ap' nam') = op where
    eaten = eat-leave+eat'
    left = if eaten <= 0 then leave+leave' else leave'
    op = OP {eats=max 0 eaten, leaves=left, applyOp=ap' . ap, name=nam++nam'}

wrap0 = (:)
wrap1 op [] = []
wrap1 op (a:as) = op a : as
wrap2 op [] = []
wrap2 op [a] = [a]
wrap2 op (a:a':as) = (a `op` a') : as
wrap3 op [] = []
wrap3 op [a] = [a]
wrap3 op as@(_:_:[]) = as
wrap3 op (a:a':a'':as) = op a a' a'' : as

wrapT n sym as = if length as >= n then Node sym (take n as) : drop n as else as
treeOP n sym = OP {eats=n, leaves=1, applyOp=wrapT n sym, name=show sym}
treeConst sym = treeOP 0 sym

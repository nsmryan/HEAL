module OrderTree (
  ordTreeSyms,
  evalOrdTree,
) where

import EAMonad
import Operators
import Data.Tree

ordTreeSyms n = (ops, terms) where
  ops = map (treeOP 2) nums
  terms = map treeConst nums
  nums = [0..n-1]

evalOrdTree (Node a []) = 0
evalOrdTree (Node a (lc@(Node l _):rc@(Node r _):[])) = left + right where
  left = case compare a l of
    LT -> 1 + evalOrdTree lc
    GT -> 0
    EQ -> leftrecurse a lc
  right = case compare a r of
    LT -> 1 + evalOrdTree rc
    GT -> 0
    EQ -> leftrecurse a rc
evalOrdTree tree = error $ drawTree $ fmap show tree

leftrecurse a (Node a' []) = if a < a' then 1 else 0
leftrecurse a node@(Node a' (l:rest)) = case compare a a' of
 EQ -> leftrecurse a l
 LT -> 1 + evalOrdTree node
 GT -> 0

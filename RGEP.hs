{-# LANGUAGE BangPatterns #-}
module RGEP(
  rgepeval,
  cdns2syms,
  cdnlen,
  rgep,
  rgeprecomb
) where

import Operators(OP)
import Postfix(postfix)
import EA(ga, evaluate, maxGens, rndPopFrom)
import Selection(tournament, elitism, bestInd)
import Recombine(mutation, rotation, crossover)
import Randomly
import Linear as L
import Data.List
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

rgepeval ops terms ind = postfix (cdns2syms cs ops terms ind) where
  cs = cdnlen (length ops) (length terms)

rgeprecomb cs pm pr pc1 pc2 pop = 
  mutation pm pop >>= rotation cs pr >>= 
  crossover 1 pc1 >>= crossover 2 pc2

rgep ps is pm pr pc1 pc2 gens ops terms eval = let
  cs = cdnlen (length ops) (length terms) in
    ga (rndPopFrom ps (is*cs) True)
       (evaluate (eval . rgepeval ops terms))
       tournament
       (rgeprecomb cs pm pr pc1 pc2)
       True
       (maxGens gens)

oneOf a b c = if b then c else a
updateIdx idx b = (2*idx) + fromEnum b
cdns2syms cs ops terms cdns = symlist where
  (_, _, _, symlist) = F.foldl' toInts (cs, 0, oneOf ops (L.head cdns) terms, []) cdns
  indexInto symset idx = symset !! (idx `mod` length symset)
  toInts (!cnt, !idx, !symset, !l) b = if cnt == cs then
    (1, 0, oneOf ops b terms, l) else
    let idx' = updateIdx idx b in
      (cnt+1, idx', symset, if cnt==cs-1 then indexInto symset idx' : l else l)

-- 1+ceil(log2(max(|ops|, |terms|)))
cdnlen :: Int -> Int -> Int
cdnlen ops terms = (1+) $ ceiling $ logBase 2 (fromIntegral $ max ops terms)

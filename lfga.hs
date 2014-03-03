{-# LANGUAGE BangPatterns #-}
module Main (
  main
) where

import EA
import EAMonad
import Randomly
import Postfix
import Maybe
import SymReg
import Selection
import Operators
import LinF
import qualified GeneticOperators as G
import Control.Monad
import Data.Monoid
import Data.List(foldl')
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.Traversable as Tr

fRndPop ps is = fmut 0.5 $ finit ps (finit is False)

rndPop ps is = do
  inds <- V.replicateM ps $! V.replicateM is nextBool
  return $! wrapPop inds

wrapV v = linf (V.length v) $ \i -> v V.! i
wrapPop pop = wrapV $ V.map wrapV pop
reify (LinF s f) = let pop = V.generate s (\y -> V.generate (fsize (f y)) (\ x -> findex (f y) x)) in
  -- ugly strictness hack
  (V.foldl (\p i -> (V.foldl (\i l -> l `seq` i ) i i) `seq` p) pop pop) `seq` pop

rewrap = wrapPop . reify 

orOn f b g = if b then g else f

--mutation
fmut pm (LinF s pop) = do
  let indlen = G.count (pop 0)
  let ms = ceiling $ (pm *) $ fromIntegral (s*indlen)
  muts <- replicateM ms $ nextInt s
  pnts <- replicateM ms $ nextInt indlen
  let addlocus map i l = M.insertWith S.union i (S.singleton l) map
  let mapping = foldl' (\ map (i, p) -> addlocus map i p) M.empty (zip muts pnts)
  return $ linf s $ \y -> let (LinF s' ind) = pop y in
    linf s' $ \x -> maybe (ind x) (\ set -> orOn id (S.member x set) not (ind x)) (M.lookup y mapping)

fcross1 pc1 (LinF s pop) = do
  let indlen = G.count (pop 0)
  let cs = ceiling $ ((pc1 / 2.0) *) $ fromIntegral s
  rands <- shuffle [0..s-1]
  let (first, second) = splitAt cs $ take (2*cs) rands
  cps <- replicateM cs (nextInt cs)
  let firsts = M.fromList $ zip first $ zip second cps
  let seconds = M.fromList $ zip second $ zip first cps
  let inrange x y (y', cap) = if x < cap then y else y'
  let notinrange x y (y', cap) = if x >= cap then y else y'
  let getGuy x y set t = fmap (t x y) (M.lookup y set)
  let find y x = getGuy x y firsts inrange  `mplus` getGuy x y seconds notinrange
  return $ linf s $ \y -> let (LinF s' ind) = pop y in 
    linf s' $ \x -> maybe (ind x) (\y -> G.index (pop y) x) (find y x)

tournselect (LinF s pop) = do
  pop' <- V.replicateM s $ tourny pop s
  return $ linf s $ \y -> fst $ pop (G.index pop' y)
tourny p s = do
  i <- nextInt s
  i' <- nextInt s
  b <- test 0.75
  let f = snd $ p i
  let f' = snd $ p i'
  let better = if f > f' then i else i'
  let worse = if f <= f' then i else i'
  return $ if b then better else worse

--rgeprecomb pm pc1 pop = fmut pm pop >>= fcross1 pc1  >>= return . rewrap
rgeprecomb pm pc1 pop = fmut pm pop >>= fcross1 pc1
  
ps = 1000
is = 100
gens = 30
pm = 0.02
pc1 = 0.7

main = do
  (p, e, l, g)  <- runEAIO (galf ps is pm pc1 gens eval) ()
  writeFile "galftest" l

eval i = return $ F.foldl (\fit b -> (fit*2) + if b then 1 else 0) 0 i

galf ps is pm pc1 gens eval = 
  ga (rndPop ps is)
     (evaluate eval)
     tournselect
     (rgeprecomb pm pc1 )
     False
     (maxGens gens)

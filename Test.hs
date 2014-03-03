{-# LANGUAGE BangPatterns #-}
module Main (
  main,
  rndPop,
  rgeprecomb,
  wrapPop,
  reify,
  wrapV,
  fRndPop
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
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.IntSet as S
import qualified Data.IntMap as M
import qualified Data.Traversable as Tr

codonLength :: [a] -> [b] -> Int
codonLength ops terms = (1+) $ ceiling $ logBase 2 (fromIntegral $ max (length ops) (length terms))

--create pop
rndPop ps is cs = do
  inds <- V.replicateM ps $! V.replicateM (is*cs) nextBool
  return $! wrapPop inds

fRndPop ps is cs = fmut 0.5 $ finit ps (finit (is*cs) True)

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
  let mapping = foldl (\ map (i, p) -> addlocus map i p) M.empty (zip muts pnts)
  return $ linf s $ \y -> let (LinF s' ind) = pop y in
    linf s' $ \x -> maybe (ind x) (\set -> orOn id (S.member x set) not (ind x)) (M.lookup y mapping)

frot pr cs (LinF s pop) = do
  let indlen = G.count (pop 0)
  let rs = ceiling $ (pr *) $ fromIntegral s
  inds <- replicateM rs $ nextInt s
  rawrots <- forM inds $ \ _ -> nextInt indlen
  let rots = map (\rp -> (rp `div` cs) * cs) rawrots
  let mapping = M.fromList $ zip inds rots
  return $ linf s $ \ y -> let (LinF s' ind) = pop y in
    linf s' $ \x -> maybe (ind x) (\i -> ind $ (i+x) `mod` s') (M.lookup y mapping)

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

fcross2 pc2 pop = do
  pop' <- fcross1 pc2 pop
  fcross1 pc2 pop'

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

rgeprecomb cs pm pr pc1 pc2 pop = 
  fmut pm pop >>= frot pr cs >>= 
  fcross1 pc1 >>= fcross2 pc2 >>= return . rewrap
  
ps = 1000
is = 100
gens = 500
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
main = do
  (p, e, l, g)  <- runEAIO (rgep ps is pm pr pc1 pc2 gens ops terms eval) ()
  writeFile "rtestlf" l

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]
f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
testcases = uniformCases (-10, 10) 21 f
eval = return . resError testcases 10000

rgep ps is pm pr pc1 pc2 gens ops terms eval = let cs = codonLength ops terms in
  --ga (fRndPop ps is cs)
  ga (rndPop ps is cs)
     (\p -> printBest p >>= evaluate (eval . postfix . cdns2syms cs ops terms))
     tournselect
     (rgeprecomb cs pm pr pc1 pc2)
     True
     (maxGens gens)

cdns2syms cs ops terms cdns = sym : syms where
  index = F.foldl' (\i b -> i*2 + if b then 0 else 1) 0 $ G.rest (G.take cs cdns)
  symset = if G.first cdns then ops else terms
  sym = symset !! (index `mod` length symset)
  syms = if G.isEmpty cdns' then [] else cdns2syms cs ops terms cdns'
    where cdns' = G.drop cs cdns

bitsToString s = F.foldl' (++) "" (fmap (\b -> if b then "1" else "0") s)
printBest pop = do
  gens <- getGens
  if gens `mod` 50 == 0 then record (F.foldl' (\a b -> b ++ "\n" ++ a) "" (fmap bitsToString pop)) else record ""
  return $ pop


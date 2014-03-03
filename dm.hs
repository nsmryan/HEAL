module Main (
  main
) where

import EA
import RGEP
import EAMonad
import Postfix
import Multiplex
import DecisionTrees 

exps = 1
ps = 1000
is = 100
gens = 500
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
k = 3
maxFit = 100000.0


ops = decisions ([(2, "a"++show i) | i <- [0..k-1]] ++ [(2, "d"++show i) | i <- [0..(2^k)-1]])
terms = [constant 0, constant 1]

tests = fmap (\(bs, b) -> (fmap b2i bs, b2i b)) $ testcases k
b2i b = if b then 1 else 0
eval Nothing = return $ 0
eval (Just f) = return $ evaltree tests f

run = rgep ps is ops terms pm pr pc1 pc2 eval gens

main = do
  result <- experiment exps ""
  writeFile "dmtest" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  --let (i, f) = best p
  --let ind = (drawAsTree $ fromJust $ postfix (cnds2symbols ops terms i)) ++ " with fitness: " ++ show f
  --let ind = show (cnds2symbols ops terms i) ++ " with fitness: " ++ show f
  experiment (times-1) $ result ++ l -- ++ ind


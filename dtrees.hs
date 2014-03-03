module Main(
  main
) where

import EA
import RGEP
import EAMonad
import Postfix
import DecisionTrees

exps = 1
ps = 100
is = 30
gens = 50
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
worst = 0

-- sunny overcast rainy, hot mild cool, high normal, false true, no yes
tests = [([0,0,0,0], 0), ([0,0,0,1], 0), ([1,0,0,0], 1), ([2,1,0,0], 1), ([2,2,1,0], 1), ([2,2,1,0], 1), ([1,2,1,1], 1), ([0,1,0,0], 0), ([0,2,1,0], 1), ([2,1,1,0], 1), ([0,1,1,1], 1), ([1,1,0,1], 1), ([1,0,1,0], 1), ([2,1,1,1], 0)] 
ops = [decide 3 0 "outlook", decide 3 1 "temp", decide 2 2 "humid", decide 2 3 "play"]
terms = [constant 0, constant 1]

eval Nothing = return worst
eval (Just f) = return $ evaltree tests f

run = rgep ps is ops terms pm pr pc1 pc2 eval gens

main = do
  result <- experiment exps ""
  writeFile "dtreetest" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  experiment (times-1) $ result ++ l -- ++ ind

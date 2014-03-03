module Main(
  main
) where

import RGEP
import SymReg
import EA
import EAMonad
import Postfix
import Selection

exps = 1
ps = 200
is = 80
gens = 200
pm = 0.00
pr = 0.02
pc1 = 0.7
pc2 = 0.7
maxFit = 100000.0

ops = [plus, minus, mult, divide]--, dup, over, drp, nip, tuck, swap] --, logx, expx, power, sinx, cosx]
terms = [var]

f x = (x^4) + (x^3) + (x^2) + (x)
--f x = 5*(x ^ 4) + 4*(x^3) + 3*(x^2) + 2*x + 1
--testcases = uniformCases (-10, 10) 21 f
testcases = map (\i->(f i, i)) [2,81, 6, 7.043, 8, 10, 11.38, 12, 14, 15, 20]
eval = return . resError testcases maxFit

run = rgep ps is pm pr pc1 pc2 gens ops terms eval 

main = do
  result <- experiment exps ""
  writeFile "rtestgep" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  --(p, e, l, g) <- runEAIO run (Min maxFit) --()
  --let (i, f) = best p
  --let ind = (drawAsTree $ fromJust $ postfix (cnds2symbols ops terms i)) ++ " with fitness: " ++ show f
  --let ind = show (cnds2symbols ops terms i) ++ " with fitness: " ++ show f
  experiment (times-1) $ result ++ l -- ++ ind

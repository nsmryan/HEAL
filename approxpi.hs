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
ps = 1000
is = 200
gens = 100
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
worst = Min 100000.0

ops = [plus, minus, mult, divide, sinx, cosx, logx, expx]--, dup, over, drp, nip, tuck, swap] --, logx, expx, power, sinx, cosx]
terms = constants [1, 2, 3, 4, 5, 6, 0.5, 0.1]

eval Nothing = return worst
eval (Just f) = return $ Min $ abs $ (f 0) - pi

run = rgep ps is pm pr pc1 pc2 gens ops terms eval 

main = do
  result <- experiment exps ""
  writeFile "rpi" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  --let (i, f) = best p
  --let ind = (drawAsTree $ fromJust $ postfix (cnds2symbols ops terms i)) ++ " with fitness: " ++ show f
  --let ind = show (cnds2symbols ops terms i) ++ " with fitness: " ++ show f
  experiment (times-1) $ result ++ l -- ++ ind

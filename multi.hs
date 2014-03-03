module Main(
  main
) where

import EA
import RGEP
import PGEP
import EAMonad
import Postfix
import Multiplex

exps = 1
ps = 2000
is = 150
gens = 3000
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
k = 3
worst = 0

ops = [ifthenOP]--, andOP, orOP, notOP]
terms = registers (k+2^k)

tests = multicases k
eval = countCorrect tests worst

run = rgep ps is pm pr pc1 pc2 gens ops terms eval 
--run = pgep ps is pm pr pc1 pc2 gens eval (ops ++ terms)

main = do
  result <- experiment exps ""
  writeFile "mtest11r" result

experiment 0 l = return l
experiment times result = do
  (p, e, l, g) <- runEAIO run ()
  experiment (times-1) $ result ++ l -- ++ ind

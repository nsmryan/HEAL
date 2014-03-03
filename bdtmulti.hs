module Main(
  main,
  ops,
  terms,
  tests
) where

import RGEP
import PGEP
import SymReg
import EA
import EAMonad
import Postfix
import Randomly
import Operators
import Selection
import BDT
import Multiplex
import Data.List
import Data.Function
import Control.Monad

exps = 1
ps = 2000
is = 60
gens = 100
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
x = 10000
k = 3
regs = k + 2^k

ops = [decide (show i ++ b) | i <- [1..regs], b <- ["True", "False"]]
terms = [decided "True", decided "False"]

tests = map (\(bs, b) -> (zipWith (\i b -> show i ++ show b) [1..length bs] bs, show b)) $ multicases k

eval Nothing = return 0
eval (Just tree) = return $ penalty $ evalcases tree tests --sizescale (treedepth tree) $ penalty $ evalcases tree tests
penalty n = if n <= 2^k then 0 else n
sizescale s c = (c^2) * (x/((s^2)+x))

run = rgep ps is pm pr pc1 pc2 gens ops terms eval
--run = pgep ps is pm pr pc1 pc2 gens ops terms eval 

main = do
  result <- replicateM exps experiment
  writeFile "btest" $! foldl' (++) "" result

experiment = do
  (p, e, l, g) <- runEAIO run ()
  --putStrLn $ if bestFit p == sizescale 7 8 then "success" else "failure"
  print $ rgepeval $ cdns2syms (cdnlen ops terms) ops terms $ bestInd p
  --print $ pgepeval $ bestInd p
  let (Just bestguy) = rgepeval $ cdns2syms (cdnlen ops terms) ops terms $ bestInd p
  print $ penalty $ evalcases bestguy tests
  putStrLn $ "With depth: " ++ show (treedepth bestguy)
  putStrLn $ "and size: " ++ show (treesize bestguy)
  return $! l

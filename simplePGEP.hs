module Main(
  main
) where

import SymReg
import PGEP
import EA
import Selection
import Operators
import Control.Monad.State
import qualified Data.Sequence as S
import qualified Data.Tree as T
import qualified Data.Map as M

exps = 10
ps = 1000
is = 100
gens = 500
pm = 0.02
pr = 0.02
pc1 = 0.7
pc2 = 0.7
maxFit = Min 100000

ops = basicSymRegOps
terms = symRegTerms
syms = ops ++ terms

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
--f a = (4.251*a*a) + (log (a*a)) + (7.243*exp a)
testcases = uniformCases (-10, 10) 21 f
--testcases = uniformCases (-1, 1) 20 f

eval Nothing = error "Empty individual during PGEP run"
eval (Just f) = let Min res = resError testcases f M.empty in
  return $ if isNaN res || isInfinite res then maxFit else Min res

scale p = do
  scl <- getEnv
  let genbest = bestFit p
  let scaler = unMin scl
  let p' = fmap (\(a, (Min d)) -> (a, scaler / (scaler+d))) p
  putEnv $ max scl genbest
  return $! p'

run = ga (pgepPop ps is (PSet (syms !! 0) syms))
         (evaluate (eval . pgepeval))
         (\p-> scale p >>= roulette)
         (pgeprecomb pm pr pc1 pc2)
         True
         (maxGens gens)

main = do
  result <- replicateM exps experiment
  writeFile "ptest" $ foldl (++) "" result

experiment = do
  (p, e, l, g) <- runEAIO run maxFit
  return $! l ++ "\n" 

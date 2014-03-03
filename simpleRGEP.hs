module Main(
  main,
  testcases,
  eval
) where

import RGEP
import PGEP
import SymReg
import EA
import Postfix
import Selection
import Data.List
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Tree as T
import qualified Data.Map as M

exps = 10
ps = 1000
is = 100
gens = 500
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
maxFit = Min 100000.0
numops = length ops
numterms = length terms

--ops = [plus, minus, mult, divide, dup, over, drp, nip, tuck, swap] 
ops = basicSymRegOps
terms = symRegTerms
--terms = [var]

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
--f x = (x^3) - (0.3*x^2) - (0.4*x) - (0.6)
--f x = x ^ 4 + x^3 + x^2 + x
--f x = x^2 + 3*x + 1
--f a = (4.251*a*a) + (log (a*a)) + (7.243*exp a)
--f x = 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
--testcases = uniformCases (-1, 1) 20 f
testcases = uniformCases (-10, 10) 21 f
eval Nothing = return maxFit
eval (Just f) = let fit@(Min res) = resError testcases f M.empty in
  return $ if isNaN res || isInfinite res then maxFit else fit

run = rgep ps is pm pr pc1 pc2 gens ops terms eval 

main = do
  result <- replicateM exps experiment
  writeFile "rtest" $! foldl' (++) "" result

experiment = do
  (p, e, l, g) <- runEAIO run ()
  return $! l ++ "\n"

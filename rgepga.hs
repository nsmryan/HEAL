module Main(
  main
)
where

import EA
import EAMonad
import RGEP
import PGEP
import GeneticOperators
import SymReg
import Recombine
import Selection
import Randomly
import Multiplex
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Foldable as F

exps = 10
ps = 1000
is = 100
gens = 500
pm = 0.002
pc1 = 0.7
maxFit = Min 10000

ops = [plus, minus, mult, divide]
terms = [var] ++ constants [1, 2, 3, 5, 7]

f x = (3*((x+1)^3)) + (2*((x+1)^2)) + (x+1)
testcases = uniformCases (-10, 10) 20 f

eval Nothing = return maxFit
eval (Just f) = let Min res = resError testcases f in
  return $ if isNaN res || isInfinite res then maxFit else Min res

rndPop ps is = replicateM ps $! replicateM (4*is) $ nextBool

garecomb pm pc pop = mutation pm pop >>= crossover 1 pc

gamult ps is pm pc gens eval =
  ga (rndPop ps is)
     (evaluate (eval . rgepeval . cdns2syms 4 ops terms))
     tournament
     (garecomb pm pc)
     True
     (maxGens gens)

main = do
  result <- replicateM exps experiment
  writeFile "rgepga3" $! foldl (++) "" result

experiment = do
  (p, e, l, g)  <- runEAIO (gamult ps is pm pc1 gens eval) ()
  return $! l

module Main(
  main
)
where

import EA
import EAMonad
import RGEP
import PGEP
import GeneticOperators
import Recombine
import Selection
import Randomly
import Multiplex
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Foldable as F

ps = 1000
is = 27
gens = 500
pm = 0.002
pc1 = 0.7
pr = 0.02
k = 3
worst = 0

tests = multicases k
eval ind = countCorrect tests worst $ trans ind

trans ind = rgepeval $ map point ind ++ (replicate 14 ifthenOP)

syms = mkSymbols $ registers (k+2^k)

rndPop ps is = do
  is <- replicateM ps $! replicateM is $ nextInt (length syms)
  return $ map (map (syms!!)) is

garecomb pm pc pop = mutation pm pop >>= rotation 1 pr >>= crossover 1 pc

gamult ps is pm pc gens eval =
  ga (rndPop ps is)
     (evaluate eval)
     tournament
     (garecomb pm pc)
     False
     (maxGens gens)

main = do
  (p, e, l, g)  <- runEAIO (gamult ps is pm pc1 gens eval) ()
  writeFile "gatest" l

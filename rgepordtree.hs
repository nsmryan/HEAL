module Main(
  main
) where

import RGEP
import PGEP
import OrderTree
import EA
import EAMonad
import Postfix
import Selection
import Data.List
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Map as M

exps = 1
ps = 500
is = 100
gens = 200
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
n = 5

(ops, terms) = ordTreeSyms n

eval Nothing = return 0
eval (Just tree) = return $ evalOrdTree tree

run = rgep ps is pm pr pc1 pc2 gens ops terms eval 

main = do
  result <- replicateM exps experiment
  writeFile "ordertree" $! foldl' (++) "" result

experiment = do
  (p, e, l, g) <- runEAIO run ()
  --print $ rgepeval ops terms (bestInd p)
  print $ bestFit p
  return $! l

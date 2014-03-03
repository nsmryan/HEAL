module Main(
  main
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
import qualified Linear as L
import qualified Data.List.Split as S
import Data.Tree as T
import Data.List
import Data.Function
import Control.Monad
import Data.Maybe

exps = 10
ps = 200
is = 40
gens = 200
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
x = 10000
k = 5

ops = map (uncurry treeOP) [(2, "xor"), (2, "or"), (1, "not"), (2, "and")]
terms = map (treeOP 0) $ map show [0..9]
cs = cdnlen (length ops) (length terms)

evalTree (Node op ns) vars = let 
    a = evalTree (ns !! 0) vars
    a' = evalTree (ns !! 1) vars in
      case op of
        "xor" -> a `xor` a'
        "and" -> a && a'
        "or" -> a || a'
        "not" -> not a
        otherwise -> vars !! (read op)
evalcases testset tree = sum $ map (fromIntegral . fromEnum . evaltest) testset where
  evaltest (vars, expected) = evalTree tree vars == expected

xor True a = not a
xor False a = a
makecases = do
  vs <- replicateM 100 $ replicateM 10 nextBool
  let equ v = ((v!!0) `xor` (v!!1)) || ((v!!2) `xor` (v!!3))
  --let equ v = ((v!!0) `xor` (v!!1)) `xor` ((v!!2) `xor` (v!!3))
  --let equ v = ((v!!0) `xor` (v!!1)) || ((v!!2) && (v!!3)) || ((v!!4) && (v!!5))
  --let equ v = (v!!0) `xor` (v!!1) `xor` (v!!2)
  --let equ v = (v!!0) `xor` (v!!1) `xor` (v!!2) `xor` (v!!3)
  let processed = zip vs $ map equ vs
  --return $ foldl (\set i -> L.adjust (\(train, res) -> (train, if res == "T" then "F" else "T")) i set) processed [0,10..90]
  return processed 

parition k cases = let partsize = length cases `div` k in
  map groupcase [L.break i (S.splitEvery partsize cases) | i <- [0..k-1]]
groupcase (h, c, t) = (concat h ++ concat t, c)


eval _ Nothing = return 0
--eval testcases (Just tree) = return $ evalcases testcases tree 
eval testcases (Just tree) = return $ sizescale (fromIntegral $ length $ levels tree) $ evalcases testcases tree
sizescale s c = (c^2) * (x/((s^2)+x))

run testcases = rgep ps is pm pr pc1 pc2 gens ops terms (eval testcases)
--run = pgep ps is pm pr pc1 pc2 gens ops terms eval 

average as = sum as / fromIntegral (length as)
main = do
  result <- replicateM exps $ experiment k
  let avged = average result
  putStrLn $ "avg error: " ++  show avged
  writeFile "btest" $ drop 1 $ foldl1' (++) $ map (("\nBestFit: " ++) . show) result

getError testset training run = do 
  ps <- mapM (const (run training)) [0..9]
  let getTree ind = fromJust $ rgepeval ops terms ind
  let getBest = maximumBy (\a b -> compare (snd a) (snd b))
  let bestTree = fst $ getBest $ map (\p -> (getTree $ bestInd p, bestFit p)) ps
  let err = fromIntegral (evalcases testset bestTree) / fromIntegral (length testset)
  --putStrLn $ "tree depth: " ++ show (treedepth bestTree) ++ "\nwith nodes: " ++ show (treesize bestTree)
  --print $ evalcases testset bestTree
  --print $ (extra-) $ evalcases testset bestTree
  --print bestTree
  --return $ maximum $ map (\i -> fromIntegral (evalcases training bestTree) / fromIntegral (length training)) $ map bestInd ps
  return err

experiment k = do
  tests <- evalEAIO makecases ()
  let runtest (train, test) = evalEAIO (getError test train run) ()
  errs <- mapM runtest $ parition k tests
  print $ sum errs / fromIntegral (length errs)
  return $ average errs
  --putStrLn $ if bestFit p == sizescale 7 8 then "success" else "failure"
  --print $ rgepeval $ cdns2syms ops terms $ bestInd p
  --print $ pgepeval $ bestInd p
  --return $! l

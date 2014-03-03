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
import BDT
import qualified Linear as L
import qualified Data.List.Split as S
import Data.List
import Data.Function
import Control.Monad
import Data.Maybe

exps = 10
ps = 200
is = 50
gens = 200
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
x = 10000
k = 5

ops = [decide (b ++ show i) | i <- [1..10], b <- ["T", "F"]] --["A1T", "A1F", "A2T", "A2F", "A3T", "A3F"]
terms = [decided "T", decided "F"]
cs = cdnlen (length ops) (length terms)

testcases = [(["A1T", "A2F", "A3T"], "T"),
             (["A1T", "A2F", "A3F"], "T"),
             (["A1F", "A2T", "A3F"], "T"),
             (["A1F", "A2T", "A3T"], "T"),
             (["A1F", "A2F", "A3F"], "F"),
             (["A1F", "A2F", "A3F"], "F"),
             (["A1T", "A2T", "A3T"], "F"),
             (["A1T", "A2T", "A3F"], "F")]

b2str b = if b then "T" else "F"
xor True a = not a
xor False a = a
makecases = do
  vs <- replicateM 100 $ replicateM 10 nextBool
  let stringed = map (zipWith (\i b -> b2str b ++ show i) [1..10]) vs
  let equ v = ((v!!0) `xor` (v!!1)) || ((v!!2) && (v!!3))
  --let equ v = ((v!!0) `xor` (v!!1)) `xor` ((v!!2) && (v!!3))
  --let equ v = ((v!!0) `xor` (v!!1)) || ((v!!2) && (v!!3)) || ((v!!4) && (v!!5))
  --let equ v = (v!!0) `xor` (v!!1) `xor` (v!!2)
  --let equ v = (v!!0) `xor` (v!!1) `xor` (v!!2) `xor` (v!!3)
  let processed = zip stringed $ map (\v -> take 1 (show (equ v))) vs
  --return $ foldl (\set i -> L.adjust (\(train, res) -> (train, if res == "T" then "F" else "T")) i set) processed [0,10..90]
  return processed 

parition k cases = let partsize = length cases `div` k in
  map groupcase [L.break i (S.splitEvery partsize cases) | i <- [0..k-1]]
groupcase (h, c, t) = (concat h ++ concat t, c)


eval _ Nothing = return 0
--eval testcases (Just tree) = return $ evalcases testcases tree 
eval testcases (Just tree) = return $ sizescale (treedepth tree) $ evalcases testcases tree 
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
  let getTree ind = fromJust $ rgepeval $ cdns2syms cs ops terms ind
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

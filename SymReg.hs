{-# LANGUAGE BangPatterns #-}
module SymReg (
  uniformCases,
  evalsymreg,
  resError,
  symRegOps,
  symRegTerms,
  basicSymRegOps
) where

import Operators
import Selection(Min(..))
import Data.Tree
import qualified Data.Map as M

sanitize n = if isInfinite n || isNaN n then 1 else n
pow a b    = sanitize $ a ** b
exp'       = sanitize . exp
logsafe n  = if n <= 0 then 0 else log n
a // b     = if b == 0 then 0 else a / b

basicSymRegOps = ops 2 ["+", "-", "*", "/"]
symRegOps = basicSymRegOps ++ ops 1 ["sin", "cos", "log", "exp"]
ops n = map (treeOP n)

symRegTerms = map treeConst ["x", "1", "2", "3", "5", "7"]

evalsymreg (Node !a []) map = case M.lookup a map of
  Just res -> res
  Nothing -> read a
evalsymreg (Node !a [a']) map = let 
  !c1 = evalsymreg a' map 
  in case a of
    "sin" -> sin c1
    "cos" -> cos c1
    "exp" -> exp' c1
    "log" -> logsafe c1
    otherwise -> error $ "Found symbol: " ++ show a
evalsymreg (Node !a as) map = let 
  !c1 = evalsymreg (as !! 0) map 
  !c2 = evalsymreg (as !! 1) map 
  in case a of
    "+" -> c1 + c2
    "-" -> c1 - c2
    "*" -> c1 * c2
    "/" -> c1 // c2
    "^" -> pow c1 c2
    otherwise -> error $ "Found symbol: " ++ show a

uniformCases :: (Double, Double) -> Int -> (Double -> Double) -> [(Double, Double)]
uniformCases (start, end) num f = map makeCase [0..fromIntegral num-1.0]
   where diff = end - start
         inc = diff / fromIntegral num
         makeCase x = (value, f value) where value = start + (x*inc)

resError cases f map = Min $ eval cases f map

eval cases tree vars = sum $ map diff cases where
  diff (v, fv) = abs $ fv - value where
    value = evalsymreg tree (M.insert "x" v vars)

module Multiplex (
  countCorrect,
  multicases,
  register,
  registers,
  ifthenOP,
  notOP,
  orOP,
  andOP
) where

import Operators
import Data.List

addrs 0 = [[]] 
addrs k = [True:ad | ad <- ads] ++ [False:ad | ad <- ads] where
  ads = addrs (k-1)

answer k bs = bs !! (k + toInt (take k bs))

toInt bs = sum $ zipWith (\index i -> if i then 2^index else 0) [0..length bs] bs

multicases k = map (\bs -> (bs, answer k bs)) (addrs (k+2^k))

countCorrect _ worst Nothing = return worst
countCorrect tests _ (Just f) = return $ foldl' evalcase 0 tests where
  evalcase fit (bs, b) = if f bs == b then fit+1 else fit

register i = OP {eats=0, leaves=1, applyOp=((!! i):), name=show i}
registers k = map register [0..k-1]

type Multiplexer = [Bool] -> Bool

ifthen' :: Multiplexer -> Multiplexer -> Multiplexer -> [Bool] -> Bool
ifthen' b f f' bs = if b bs then f bs else f' bs 
not' :: Multiplexer -> [Bool] -> Bool
not'      f    bs = not (f bs)
or' :: Multiplexer -> Multiplexer -> [Bool] -> Bool
or'       f f' bs = f bs || f' bs
and' :: Multiplexer -> Multiplexer -> [Bool] -> Bool
and'      f f' bs = f bs && f' bs

--wrap1 f (a:as) = f a:as
--wrap2 f (a:a':as) = f a a':as
--wrap3 f (a:a':a'':as) = f a a' a'':as

ifthenOP = OP {eats=3, leaves=1, applyOp=wrap3 ifthen', name="ifthen"}
notOP    = OP {eats=1, leaves=1, applyOp=wrap1 not',    name="not"   }
orOP     = OP {eats=2, leaves=1, applyOp=wrap2 or',     name="or"    }
andOP    = OP {eats=2, leaves=1, applyOp=wrap2 and',    name="and"   }

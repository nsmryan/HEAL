module DecisionTrees (
  constant,
  evaltree,
  decide,
  decisions
) where

import Operators
import Data.List(foldl')

constant k = OP {eats=0, leaves=1, applyOp=(const k:), name=show k}

choseFrom n fs vars = (fs !! (vars !! n)) vars
decideOn k n fs = choseFrom n (take k fs):drop k fs
decide k n nam = OP {eats=k, leaves=0, applyOp=decideOn k n, name=nam}
decisions ps = zipWith (\(k, nam) n -> decide k n nam) ps [0..length ps-1]

evaltree testcases f = foldl' (\fit (vars, ans) -> if f vars == ans then fit+1 else fit) 0 testcases

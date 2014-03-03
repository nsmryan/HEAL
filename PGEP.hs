{-# LANGUAGE BangPatterns #-}
module PGEP(
  pgepeval,
  pgep,
  pgeprecomb,
  valid,
  validly,
  validate,
  validlyM,
  pgepPop,
  PSet(..),
  uncode
) where

import EA
import Recombine
import Operators
import Pairable
import Linear as L
import Randomly
import Postfix(postfix)
import Selection(elitism, roulette)
import Recombine(mutation, rotation, crossover)
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

-- Pointed set represented as a list and a distinguished element.
data PSet a = PSet { point::a, unpoint::[a] } deriving Show
pointin i = PSet i [i]
pointed i is = PSet i is

instance Randomizable (PSet a) where
  generateFrom (PSet _ as) = do
    a <- selectFrom as
    return $ PSet a as

instance Functor PSet where
  fmap f (PSet i is) = PSet (f i) (map f is)

instance (Eq a) => Eq (PSet a) where
  (PSet a as) == (PSet a' as') = a == a' && as == as'

pgepeval = postfix . S.reverse . uncode . (fmap point)

uncode ops = L.slice 0 (uncode' 0 1) ops where
  len = count ops
  uncode' i 0 = i
  uncode' i args = if i == len then 0 else uncode' (1+i) (args - 1 + eats (index ops i))
    
pgeprecomb pm pr pc1 pc2 pop = 
  validlyM (mutation pm) pop >>= validlyM (rotation 1 pr) >>= 
  pcross 1 pc1 >>= pcross 2 pc2

pcross n pc pop = do
  pop' <- generateFrom pop
  crossed <- Tr.mapM (mightM pc (cross n)) $ pairup pop'
  return $ validate pop $ unpair crossed

pgep ps is pm pr pc1 pc2 gens ops terms eval = let syms = ops++terms in
  ga (pgepPop ps is (PSet (syms!!0) syms))
     (evaluate (eval . pgepeval))
     roulette
     (pgeprecomb pm pr pc1 pc2)
     True
     (maxGens gens)

  
pgepPop ps is sym = loop S.empty where
  loop pop = if S.length pop == ps then return pop else do
    ind <- S.replicateM is $ generateFrom sym
    if not $ valid ind then loop pop else loop (ind S.<| pop)

valid ind = check 1 $ fmap (eats . point) ind where
  check n arrs | n == 0 = True
               | isEmpty arrs = False
               | otherwise = check (n + (L.head arrs) - 1) (L.tail arrs)
pickvalid i i' = if valid i' then i' else i

validate p p' = S.zipWith pickvalid p p' 

validly f p = validate p (f p) 

validlyM f p = do
  p' <- f p
  return $ validate p p' 

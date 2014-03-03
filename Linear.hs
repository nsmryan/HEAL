module Linear (
  Linear(..)
) where

import Data.Monoid
import Data.List(splitAt)
import Control.Applicative
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
import qualified Data.Vector as V
import Prelude as P hiding (break)


-- finite linear structures
-- minimal complete definition: cut or break
class (Alternative l, Tr.Traversable l) => Linear l where
  count :: l a -> Int
  count s = F.sum $ fmap (const 1) s

  index :: l a -> Int -> a
  index s i = let (_, a, _) = break i s in a

  update :: Int -> a -> l a -> l a
  update i a s = let (h, _, t) = break i s in h <|> pure a <|> t

  adjust :: (a -> a) -> Int -> l a -> l a
  adjust f i s = update i (f (index s i)) s

  slice :: Int -> Int -> l a -> l a
  slice b e s = fst $ cut (e-b) $ snd $ cut b s

  break :: Int -> l a -> (l a, a, l a)
  break n s = let (h, t) = cut n s in (h, index s n, slice (n+1) (count s) s)

  cut :: Int -> l a -> (l a, l a)
  cut n s = let (h, a, t) = break n s in (h, pure a <|> t)

  take :: Int -> l a -> l a
  take n s = fst $ cut n s

  drop :: Int -> l a -> l a
  drop n s = snd $ cut n s

  head :: l a -> a
  head s = index s 0

  tail :: l a -> l a
  tail s = snd $ cut 1 s

  isEmpty :: l a -> Bool
  isEmpty s = count s == 0

instance Linear [] where
  count = P.length
  isEmpty = P.null
  index = (!!)
  update _ _ [] = []
  update 0 a (_:as) = a:as
  update n a (a':as) = a':update (n-1) a as
  cut n l = splitAt n l
  slice b e l = P.drop b $ P.take e l

instance Applicative S.Seq where
  pure = S.singleton
  (<*>) = S.zipWith ($)
instance Alternative S.Seq where
  empty = S.empty
  (<|>) = (S.><)

instance Linear S.Seq where
  count = S.length
  isEmpty = S.null
  index = S.index
  update = S.update
  adjust = S.adjust
  cut = S.splitAt
  take = S.take
  drop = S.drop

instance Linear V.Vector where
  head = V.head
  tail = V.drop 1
  take = V.take
  drop = V.drop
  count = V.length
  isEmpty = V.null
  index = (V.!)
  update i a v = v V.// [(i, a)]

instance Functor V.Vector where
  fmap = V.map

instance Applicative V.Vector where
  pure = V.singleton
  --fs <*> v = V.zipWith ($) fs v
  fs <*> v = V.concatMap (flip fmap v) fs

instance Alternative V.Vector where
  empty = V.empty
  (<|>) = (V.++)
      
instance Monad V.Vector where
  return = V.singleton
  v >>= f = V.concatMap f v
  
instance F.Foldable V.Vector where
  foldr = V.foldr
    
instance Tr.Traversable V.Vector where
  traverse f v = fmap V.fromList $ Tr.traverse f $ V.toList v
  mapM = V.mapM

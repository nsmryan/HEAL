module LinF (
  LinF(..),
  fempty,
  fcons,
  fsingleton,
  finit,
  fromList,
  toList,
  linf
) where

import GeneticOperators
import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
    
data LinF a = LinF {fsize::Int, findex::(Int -> a) }

linf s f = LinF s f

instance (Show a) => Show (LinF a) where
  show lf = show $ toList lf

fromList as = LinF (length as) $ \i -> as !! i
toList (LinF s f) = map f [0..s-1]

err i = error $ "LinF index out of bounds: " ++ show i
fcons a (LinF s f) = LinF (s+1) (\i -> if i == 0 then a else f (i-1))
fsingleton a = LinF 1 (\i -> if i == 0 then a else err i)
fempty = LinF 0 (\i -> err i)
finit size a = LinF size (\i -> if i >= size then err i else a)

instance Monoid (LinF a) where
  mempty = fempty
  (LinF s f) `mappend` (LinF s' f') = LinF (s+s') $ \i -> if i < s then f i else f' (i-s)

instance Functor LinF where
  fmap g (LinF s f) = LinF s (g . f)

instance Applicative LinF where
  pure a = fsingleton a
  (LinF s f) <*> (LinF s' g) = LinF (min s s') (\i -> f i $ g i)

instance Monad LinF where
  return = fsingleton
  lf >>= g = F.foldMap id $ fmap g lf

instance F.Foldable LinF where
  foldr g b (LinF s f) = F.foldr g b $ map f [0..s-1]
  foldMap g (LinF s f) = F.foldMap g $ map f [0..s-1]

instance Tr.Traversable LinF where
  traverse f = F.foldr fcons_ (pure fempty) where
    fcons_ x ys = (fcons) <$> f x <*> ys 

instance Linear LinF where
  first (LinF _ f) = f 0
  rest (LinF s f) = LinF (s-1) $ \i -> f (succ i)
  take t (LinF s f) = LinF t $ \i -> if i >= t then err i else f i
  drop d (LinF s f) = LinF (s-d) $ f . (d+)
  count (LinF s _) = s
  isEmpty (LinF s _) = s == 0
  empty = fempty
  index (LinF s f) = f
  update i a (LinF s f) = LinF s $ \x -> if i == x then a else f x
  cons = fcons

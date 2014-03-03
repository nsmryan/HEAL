module Main (main) where


import GA
import Selection
import Linear as L
import EAMonad
import Data.Foldable as F

ps = 100
is = 10
pm = 0.002
pc = 0.7
gens = 100

xor False a = a
xor True a = not a
eval individual = return $ F.sum $ fmap fromEnum xored where
  xored = [index ind i `xor` index (reverse (L.drop half ind)) i | i <- [0..half-1]]
  half = count ind `div` 2
  ind = F.toList individual

run = galf ps is pm pc gens eval

main = do
  p <- evalEAIO run ()
  print $ bestFit p
  print $ bestInd p
  

import System
import System.IO
import Data.List.Split
import Data.List
import Graphics.GD
import Control.Monad

inds = 400
indlen = 400
imgs = 11
black = rgb 0 0 0 
white = rgb 255 255 255 

process s = map addCoord $ splitEvery inds (map reverse (lines s))
addCoord box = splitEvery inds $ do
  row <- zip box [0..length box-1]
  let (r, y) = row 
  bit <- zip r [0..length r-1]
  let (c, x) = bit
  return (c == '1', x, y)

makeImage dat = do
  img <- newImage (indlen, inds)
  mapM_ (\row -> mapM_ (\(b, x, y) -> setPixel (x, y) (if b then black else white) img) row) dat
  return img
saveImage (img, i) = savePngFile ("./data/" ++ show i ++ ".png") img
  
main = do
  args <- getArgs
  let name = args !! 0
  contents <- readFile name 
  let results = process contents
  imgs <- mapM makeImage results
  mapM_ saveImage (zip imgs [0..length results-1])

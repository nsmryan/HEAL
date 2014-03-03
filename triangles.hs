{-# LANGUAGE BangPatterns #-}
import Randomly
import EAMonad
import SymReg
import Operators
import RGEP
import Data.IORef
import Data.Maybe
import Graphics.UI.GLUT as G
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Graphics.Rendering.OpenGL as O

popSize = 6
is = 50
gens = 100
pm = 0.002
pr = 0.02
pc1 = 0.7
pc2 = 0.7
moveDist = 0.005
moveAng = 0.05
timeout = 10
radius = 0.2

ops = symRegOps
terms = map treeConst ["d", "r", "x", "y", "angle", "facing"]
numops = length ops
numterms = length terms
run = rgep popSize is pm pr pc1 pc2 gens ops terms (const (return 0.0))
trans = rgepeval ops terms

main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createAWindow progName
  population <- evalEAIO run ()
  points <- newIORef starting
  pop <- newIORef $ F.toList $ fmap fst population
  displayCallback $= displayPoints points
  keyboardMouseCallback $= Just (keyboard points)
  addTimerCallback timeout (timer points pop)
  --reshapeCallback $= Just reshape
  mainLoop

createAWindow windowName = do
  createWindow windowName

reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)

keyboard points (Char 's') Down _ _ = do
  points $=! starting
keyboard points _ _ _ _ = return ()
  

starting :: [((GLfloat, GLfloat, GLfloat), GLfloat)]
starting = replicate popSize ((0.0, 0.0, 0.0), 0.0)

displayPoints points = do
  clear [ColorBuffer]
  pointSize $= 2
  ps <- get points
  drawPoints $ map fst ps
  mapM_ print (map fst ps)
  putStrLn "\n-----------\n"
  swapBuffers

drawPoints ps = renderPrimitive Points $
  mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) ps
  
timer points pop = do
  addTimerCallback timeout (timer points pop)
  ps <- get points 
  inds <- get pop
  points $=! map (uncurry movePoint) (zip ps inds)
  postRedisplay Nothing

movePoint point ind = let
  !((!x, !y, !z), !dir) = point
  contain n | n >= 1 = -1 
            | n <= -1 = 1
            | otherwise = n
  y' = moveDist*(cos dir) + y
  x' = moveDist*(sin dir) + x
  vars = M.fromList [("r", radius), ("x", x), ("y", y), ("facing", dir), ("angle", sin (x/y)), ("d", sqrt (x*x+y*x))]
  newdir = dir + evalsymreg (fromJust (trans ind)) vars in
    ((contain x', contain y', z), newdir)

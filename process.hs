import System
import System.IO
import Data.List.Split
import Data.List

fitness = "BestFit: "
process gens trials s = unlines $ map show $ map (/trials) sums where
  sums = foldr (zipWith (+)) (replicate gens 0) $ toNumbers gens lined
  lined = map (drop (length fitness)) $ filter (isPrefixOf fitness) $ lines s
toNumbers :: Int -> [String] -> [[Double]]
toNumbers n s = splitEvery n $ map read s
  
main = do
  args <- getArgs
  let name = args !! 0
  let gens = read (args !! 1)
  let trials = read (args !! 2)
  contents <- readFile name 
  let result = process gens trials contents
  putStrLn result
  writeFile ("./data/" ++ name) result

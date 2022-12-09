{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow)

intGrid :: [Char] -> [Int]
intGrid = map (\x -> read [x] :: Int)

height :: [[a]] -> Int
height = length

width :: [[a]] -> Int
width = length . head

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible grid (x, y)
  | x == 0 || y == 0 = True
  | y + 1 == length grid || x + 1 == length (head grid) = True
  | otherwise =
      let tree = getTree grid (x, y)
          aboves = map (getTree grid) $ zip (repeat x) [0 .. y - 1]
          aboveClear = all (< tree) aboves
          belows = map (getTree grid) $ zip (repeat x) [y + 1 .. (height grid) - 1]
          belowClear = all (< tree) belows
          lefts = map (getTree grid) $ zip [0 .. x - 1] (repeat y)
          leftClear = all (< tree) lefts
          rights = map (getTree grid) $ zip [x + 1 .. (width grid) - 1] (repeat y)
          rightClear = all (< tree) rights
       in aboveClear || belowClear || leftClear || rightClear

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore grid (x, y) =
  let tree = getTree grid (x, y)
      aboves = map (curry (getTree grid) x) [0 .. y - 1]
      aboveScore' = length $ takeWhile (< tree) $ reverse aboves
      aboveScore = if aboveScore' < length aboves then aboveScore' + 1 else aboveScore'

      belows = map (curry (getTree grid) x) [y + 1 .. height grid - 1]
      belowScore' = length $ takeWhile (< tree) belows
      belowScore = if belowScore' < length belows then belowScore' + 1 else belowScore'
      lefts = zipWith (curry (getTree grid)) [0 .. x - 1] (repeat y)
      leftScore' = length $ takeWhile (< tree) $ reverse lefts
      leftScore = if leftScore' < length lefts then leftScore' + 1 else leftScore'
      rights = zipWith (curry (getTree grid)) [x + 1 .. width grid - 1] (repeat y)
      rightScore' = length $ takeWhile (< tree) rights
      rightScore = if rightScore' < length rights then rightScore' + 1 else rightScore'
   in aboveScore * belowScore * leftScore * rightScore

getTree :: [[a]] -> (Int, Int) -> a
getTree grid (x, y) = (grid !! y) !! x

main :: IO ()
main = do
  input <- readFile "input/day8.txt" -- 1700,470596
  -- input <- readFile "input/sample8.txt" -- 21,8
  let grid = map intGrid $ lines input
      ys = zipWith const [0 ..] grid
      xs = zipWith const [0 ..] (head grid)
      allCoords = [(x, y) | x <- xs, y <- ys]
  print $ length $ filter id $ map (isVisible grid) allCoords
  print $ maximum $ map (scenicScore grid) allCoords

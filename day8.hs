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

getTree :: [[a]] -> (Int, Int) -> a
getTree grid (x, y) = (grid !! y) !! x

main :: IO ()
main = do
  input <- readFile "input/day8.txt"
  -- input <- readFile "input/sample8.txt" --21
  let grid = map intGrid $ lines input
      ys = zipWith const [0 ..] grid
      xs = zipWith const [0 ..] (head grid)
      allCoords = [(x, y) | x <- xs, y <- ys]
      tst = (1, 1)
  print $ getTree grid tst
  print $ isVisible grid tst
  print $ length $ filter id $ map (isVisible grid) allCoords

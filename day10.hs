{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split (chunksOf, splitOn)
import Debug.Trace (traceShow)

data Op
  = Addx Int
  | NoOp
  deriving (Show, Eq)

parseLine :: String -> Op
parseLine ('n' : _) = NoOp
parseLine ('a' : 'd' : 'd' : 'x' : ' ' : num) = Addx (read num :: Int)

runOp :: (Int, Int, Int) -> Op -> (Int, Int, Int)
runOp (cycle, x, sig) NoOp = (cycle + 1, x, sig + calcSignal cycle x)
runOp (cycle, x, sig) (Addx n) =
  let newsig = calcSignal cycle x + calcSignal (cycle + 1) x
   in (cycle + 2, x + n, newsig + sig)

calcSignal :: Int -> Int -> Int
calcSignal cycle x =
  if cycle `elem` [20, 60, 100, 140, 180, 220]
    then cycle * x
    else 0

drawPixel :: Int -> Int -> Char
drawPixel cycle x
  | cycle == x || cycle == x - 1 || cycle == x + 1 = '#'
  | otherwise = '.'

incCycle :: Int -> Int
incCycle c = if c == 39 then 0 else c + 1

rendrOp :: (Int, Int, String) -> Op -> (Int, Int, String)
rendrOp (cycle, x, screen) NoOp = (incCycle cycle, x, screen ++ [drawPixel cycle x])
rendrOp (cycle, x, screen) (Addx n) =
  let (c, scr) = (incCycle cycle, screen ++ [drawPixel cycle x])
   in (incCycle c, x + n, scr ++ [drawPixel c x])

main :: IO ()
main = do
  input <- readFile "input/day10.txt"
  -- input <- readFile "input/sample10.txt"
  let parsedLines = map parseLine $ lines input
      (_, _, screenline) = foldl rendrOp (0, 1, "") $ parsedLines
  print $ foldl runOp (1, 1, 0) parsedLines
  putStr . unlines $ chunksOf 40 screenline

-- 13220
-- ###..#..#..##..#..#.#..#.###..####.#..#.
-- #..#.#..#.#..#.#.#..#..#.#..#.#....#.#..
-- #..#.#..#.#..#.##...####.###..###..##...
-- ###..#..#.####.#.#..#..#.#..#.#....#.#..
-- #.#..#..#.#..#.#.#..#..#.#..#.#....#.#..
-- #..#..##..#..#.#..#.#..#.###..####.#..#.

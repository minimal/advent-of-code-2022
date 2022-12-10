{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split (splitOn)
import Data.Map (Map ())
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

data Op
  = Addx Int
  | NoOp
  deriving (Show, Eq)

cycles :: Set.Set Int
cycles = Set.fromList [20, 60, 100, 140, 180, 220]

parseLine :: String -> Op
parseLine ('n' : 'o' : 'o' : 'p' : _) = NoOp
parseLine ('a' : 'd' : 'd' : 'x' : ' ' : num) = Addx (read num :: Int)

runOp :: (Int, Int, Int) -> Op -> (Int, Int, Int)
runOp (cycle, x, sig) NoOp = (cycle + 1, x, sig + calcSignal (cycle, x, sig))
runOp (cycle, x, sig) (Addx n) =
  let newsig = calcSignal (cycle, x, sig) + calcSignal (cycle + 1, x, sig)
   in (cycle + 2, x + n, newsig + sig)

calcSignal :: (Int, Int, Int) -> Int
calcSignal (cycle, x, sig) =
  if Set.member cycle cycles
    then cycle * x
    else 0

input =
  "noop\n\
  \addx 3\n\
  \addx -5"

main :: IO ()
main = do
  input <- readFile "input/day10.txt"
  -- input <- readFile "input/sample10.txt"
  let parsedLines = map parseLine $ lines input
  print $ foldl runOp (1, 1, 0) parsedLines

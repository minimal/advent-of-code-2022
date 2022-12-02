{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input/day1.txt"
  let totals = map (sum . map read) . splitOn [""] . lines $ input
      pt1 = foldl max 0 totals
      pt2 = sum $ take 3 $ reverse $ sort totals
  print [pt1, pt2]

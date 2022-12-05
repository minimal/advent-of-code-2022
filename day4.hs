{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split (splitOn)

input :: String
input =
  "2-4,6-8\n\
  \2-3,4-5\n\
  \5-7,7-9\n\
  \2-8,3-7\n\
  \6-6,4-6\n\
  \2-6,4-8"

main :: IO ()
main = do
  input <- readFile "input/day4.txt"
  print $ length $ filter id $ map splitPairs $ lines input
  where
    splitPairs pairs = within $ map (map (\x -> read x :: Int) . splitOn ['-']) $ splitOn [','] pairs
    within [[a, b], [x, y]] = a >= x && b <= y || x >= a && y <= b

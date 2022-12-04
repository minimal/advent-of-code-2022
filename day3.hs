{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List.Split (chunksOf)
import qualified Data.Set as Set

input =
  "vJrwpWtwJgWrhcsFMMfFFhFp\n\
  \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
  \PmmdzqPrVvPwwTWBwg\n\
  \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
  \ttgJtRGJQctTZtZT\n\
  \CrZsJsPPZsGzwwsLwLmpwMDw"

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let prios = map winner $ lines input
      badges = map badge $ chunksOf 3 $ lines input
  print $ sum prios
  print $ sum $ map (charPrio . Set.elemAt 0) badges
  where
    winner line =
      let (a, b) = splitAt (length line `div` 2) line
       in charPrio $ Set.elemAt 0 $ Set.intersection (Set.fromList a) (Set.fromList b)
    badge triple =
      let (a : b : c : _) = map Set.fromList triple
       in Set.intersection (Set.intersection a b) c
    charPrio c = if isLower c then fromEnum c - 96 else fromEnum c - 38

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input/day6.txt"
  print $ check 0 "" input
  where
    check idx buf [] = idx
    check idx buf (x : stream)
      | length buf == 14 = idx
      | x `notElem` buf = check (idx + 1) (buf ++ [x]) stream
      | otherwise = check (idx + 1) ((splitOn [x] buf !! 1) ++ [x]) stream

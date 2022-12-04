--{-# LANGUAGE OverloadedStrings #-}

scoreRound :: String -> Int
scoreRound [a, _, b] =
  let a' = fromJust $ elemIndex a "ABC"
      b' = fromJust $ elemIndex b "XYZ"
   in b' + 1 + case (b' - a') `mod` 3 of
        1 -> 6
        0 -> 3
        _ -> 0

scoreRound2 :: String -> Int
scoreRound2 ['A',_,'X'] = 3 + 0
scoreRound2 ['A',_,'Y'] = 1 + 3
scoreRound2 ['A',_,'Z'] = 2 + 6
scoreRound2 ['B',_,'X'] = 1 + 0
scoreRound2 ['B',_,'Y'] = 2 + 3
scoreRound2 ['B',_,'Z'] = 3 + 6
scoreRound2 ['C',_,'X'] = 2 + 0
scoreRound2 ['C',_,'Y'] = 3 + 3
scoreRound2 ['C',_,'Z'] = 1 + 6

main :: IO ()
main = do
  input <- readFile "input/day2.txt"
  let rounds = lines input
      scores = map scoreRound rounds
      scores2 = map scoreRound2 rounds
  print [sum scores,sum scores2]

import Data.Char

react :: String -> String
react (u1:u2:xs)
  | u1 == u2 = u1 : react (u2:xs)
  | toUpper u1 == toUpper u2 = react xs
  | otherwise = u1 : react (u2:xs)
react xs = xs

part1 :: String -> Int
part1 input = let input' = react input
  in if input == input'
  then length input
  else part1 input'

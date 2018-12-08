import Data.Char
import Data.List

react :: String -> String
react (u1:u2:xs)
  | u1 == u2 = u1 : react (u2:xs)
  | toUpper u1 == toUpper u2 = react xs
  | otherwise = u1 : react (u2:xs)
react xs = xs

fullyReact :: String -> String
fullyReact p = let p' = react p
  in if p == p'
  then p
  else fullyReact p'

part1 :: String -> Int
part1 = length . fullyReact . head . lines

part2 :: String -> Int
part2 input = let p = fullyReact $ head $ lines input
  in minimum $ do
    let units = nub $ map toUpper $ p
    unit <- units
    let p' = filter (\c -> c /= unit && c /= toLower unit) p
    return (length $ fullyReact p')

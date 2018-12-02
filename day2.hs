import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

hasTwo :: Map Char Int -> Bool
hasTwo = not . Map.null . Map.filter (== 2)

hasThree :: Map Char Int -> Bool
hasThree = not . Map.null . Map.filter (== 3)

hasTwoOrThree :: String -> [Bool]
hasTwoOrThree line = [hasTwo m, hasThree m]
  where m = Map.fromListWith (+) [(x, 1) | x <- line]

part1 :: String -> Int
part1 input = two * three
  where [two, three] = map (sum . map fromEnum) $ transpose $ map hasTwoOrThree $ lines input

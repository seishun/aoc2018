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

similar :: String -> String -> Bool
similar (x1:xs1) (x2:xs2) = xs1 == xs2 || x1 == x2 && similar xs1 xs2

correctBoxes :: [String] -> (String, String)
correctBoxes (id1:xs) = case find (similar id1) xs of
  Just id2 -> (id1, id2)
  Nothing -> correctBoxes xs

common :: String -> String -> String
common [] [] = ""
common (x1:xs1) (x2:xs2) =
  if x1 == x2
  then x1 : common xs1 xs2
  else common xs1 xs2

part1 :: String -> Int
part1 input = two * three
  where [two, three] = map (sum . map fromEnum) $ transpose $ map hasTwoOrThree $ lines input

part2 :: String -> String
part2 input = common id1 id2
  where (id1, id2) = correctBoxes $ lines input

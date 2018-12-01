import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Int
parse ('+' : xs) = read xs
parse xs = read xs

firstDuplicate :: Int -> Set Int -> [Int] -> Int
firstDuplicate current seen (x:xs) =
  let resulting = current + x
  in if Set.member resulting seen
  then resulting
  else firstDuplicate resulting (Set.insert resulting seen) xs

part1 :: String -> Int
part1 = sum . map parse . lines

part2 :: String -> Int
part2 = firstDuplicate 0 Set.empty . cycle . map parse . lines

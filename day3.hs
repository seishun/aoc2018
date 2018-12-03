import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Claim = (Int, Int, Int, Int, Int)

addClaim :: Map (Int, Int) Int -> Claim -> Map (Int, Int) Int
addClaim areas = Map.unionWith (+) areas . Map.fromList . map (\s -> (s, 1)) . getArea

getArea :: Claim -> [(Int, Int)]
getArea (_, left, top, width, height) = do
  x <- [left .. left + width - 1]
  y <- [top .. top + height - 1]
  return (x, y)

overlaps :: Map (Int, Int) Int -> Claim -> Bool
overlaps areas = any (\s -> areas ! s > 1) . getArea

parse :: [String] -> Claim
parse ['#':id_, "@", offsets, size] =
  let [(left, ',':top_)] = reads offsets
      [(top, ":")] = reads top_ 
      [(width, 'x':height_)] = reads size
      height = read height_
  in (read id_, left, top, width, height)

part1 :: String -> Int
part1 = Map.size . Map.filter (>1) . foldl addClaim Map.empty . map (parse . words) . lines

part2 :: String -> Int
part2 input = id
  where claims = map (parse . words) $ lines input
        areas = foldl addClaim Map.empty claims
        (id, _, _, _, _) = fromJust $ find (not . overlaps areas) claims

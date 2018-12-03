import Data.Map (Map)
import qualified Data.Map as Map

type Claim = (Int, Int, Int, Int)

addClaim :: Map (Int, Int) Int -> Claim -> Map (Int, Int) Int
addClaim claims (left, top, width, height) = Map.unionWith (+) claims $ Map.fromList area
  where area = do
        x <- [left .. left + width - 1]
        y <- [top .. top + height - 1]
        return ((x, y), 1)

parse :: [String] -> Claim
parse [_, "@", offsets, size] =
  let [(left, ',':top_)] = reads offsets
      [(top, ":")] = reads top_ 
      [(width, 'x':height_)] = reads size
      height = read height_
  in (left, top, width, height)

part1 :: String -> Int
part1 = Map.size . Map.filter (>1) . foldl addClaim Map.empty . map (parse . words) . lines

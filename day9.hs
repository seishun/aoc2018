import Data.Map (Map)
import qualified Data.Map as Map

type State = ([Int], Int, Map Int Int)

turn :: State -> (Int, Int) -> State
turn (circle, current, scores) (player, number)
  | number `mod` 23 == 0 =
      let removed = (current - 7 + length circle) `mod` length circle
          score = number + circle !! removed
      in (take removed circle ++ drop (removed + 1) circle, removed, Map.insertWith (+) player score scores)
  | otherwise =
      let current' = (current + 2) `mod` length circle
      in (take current' circle ++ [number] ++ drop current' circle, current', scores)

parse :: [String] -> (Int, Int)
parse [players, "players;", "last", "marble", "is", "worth", marbles, "points"] = (read players, read marbles)

part1 :: String -> Int
part1 input =
  let (players, marbles) = parse $ words input
      (_, _, scores) = foldl turn ([0], 0, Map.empty) $ zip (cycle [1..players]) [1..marbles]
  in maximum $ Map.elems scores

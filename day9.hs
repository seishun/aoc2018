import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

type Circle = Map Int (Int, Int)
type State = (Circle, Int, Map Int Int)

turn :: State -> (Int, Int) -> State
turn (circle, current, scores) (player, number)
  | number `mod` 23 == 0 =
      let removed = iterate (fst . (circle !)) current !! 7
          (Just (left, right), circle') = Map.updateLookupWithKey (const (const Nothing)) removed circle
          circle'' = Map.insertWithKey updateRight left (0, right) $ Map.insertWithKey updateLeft right (left, 0) circle'
          score = number + removed
      in (circle'', right, Map.insertWith (+) player score scores)
  | otherwise =
      let left = snd $ circle ! current
          (Just (_, right), circle') = Map.insertLookupWithKey updateRight left (0, number) circle
          circle'' = Map.insert number (left, right) $ Map.insertWithKey updateLeft right (number, 0) circle'
      in (circle'', number, scores)
  where updateLeft _ (left, _) (_, right) = (left, right)
        updateRight _ (_, right) (left, _) = (left, right)

parse :: [String] -> (Int, Int)
parse [players, "players;", "last", "marble", "is", "worth", marbles, "points"] = (read players, read marbles)

part1 :: String -> Int
part1 input =
  let (players, marbles) = parse $ words input
      (_, _, scores) = foldl turn (Map.fromList [(0, (0, 0))], 0, Map.empty) $ zip (cycle [1..players]) [1..marbles]
  in maximum $ Map.elems scores

part2 :: String -> Int
part2 input =
  let (players, marbles) = parse $ words input
      (_, _, scores) = foldl turn (Map.fromList [(0, (0, 0))], 0, Map.empty) $ zip (cycle [1..players]) [1..marbles * 100]
  in maximum $ Map.elems scores

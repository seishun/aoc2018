import Control.Monad
import Data.Map (Map, (!?))
import qualified Data.Map as Map

data Acre = Open | Tree | Lumber deriving (Eq)
type Location = (Int, Int)
type State = Map Location Acre

parse' :: [Char] -> [Acre]
parse' (l:xs) =
  let as = parse' xs
  in case l of
    '.' -> Open:as
    '|' -> Tree:as
    '#' -> Lumber:as
parse' [] = []

parse :: String -> State
parse input =
  let as = map p . zip [1..] . map (zip [1..] . parse') . lines $ input
  in Map.fromList $ concat as
  where p (y, as) = map (\(x, a) -> ((y, x), a)) as

adjacent :: Location -> [Location]
adjacent (y, x) = do
  y' <- [y-1, y, y+1]
  x' <- [x-1, x, x+1]
  guard $ y' /= y || x' /= x
  return (y', x')

next :: State -> State
next state = Map.mapWithKey magic state
  where magic (y, x) acre =
          let trees = filter (== Just Tree) $ map (state !?) $ adjacent (y, x)
              lumber = filter (== Just Lumber) $ map (state !?) $ adjacent (y, x)
          in case acre of
            Open ->
              if length trees >= 3 then Tree else Open
            Tree ->
              if length lumber >= 3 then Lumber else Tree
            Lumber ->
              if length trees >= 1 && length lumber >= 1 then Lumber else Open

part1 :: String -> Int
part1 input =
  let state = (iterate next $ parse input) !! 10
      wood = Map.filter (== Tree) state
      lumber = Map.filter (== Lumber) state
  in Map.size wood * Map.size lumber

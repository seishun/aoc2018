import Control.Monad
import Data.Set (Set, (\\))
import qualified Data.Set as Set

type Location = (Int, Int)
data Type = Rocky | Narrow | Wet deriving (Eq)
data Tool = Torch | Gear | Neither deriving (Eq, Ord)
type State = (Location, Tool)
type Queue = (Set State, Set State, Set State, Set State, Set State, Set State, Set State)

parse :: String -> (Int, Location)
parse input =
  let ["depth:", depth_, "target:", target_] = words input
  in (read depth_, read $ "(" ++ target_ ++ ")")

risk :: (Int, Location) -> Int
risk (depth, (x, y)) =
  let cave = types (depth, (x, y))
  in sum $ do
    x' <- [0..x]
    y' <- [0..y]
    return $ case cave !! x' !! y' of
      Rocky -> 0
      Wet -> 1
      Narrow -> 2

types :: (Int, Location) -> [[Type]]
types (depth, target) = map (map t) levels
  where t level = case level `mod` 3 of
          0 -> Rocky
          1 -> Wet
          2 -> Narrow
        levels = flip map [0..] $ \x -> do
          y <- [0..]
          let index = case (x, y) of
                (0, 0) -> 0
                (_, 0) -> x * 16807
                (0, _) -> y * 48271
                (_, _) ->
                  if (x, y) == target
                  then 0
                  else levels !! (x - 1) !! y * levels !! x !! (y - 1)
          return $ (index + depth) `mod` 20183

move :: [[Type]] -> State -> [State]
move cave ((x, y), tool) = do
  x' <- [x - 1, x, x + 1]
  y' <- [y - 1, y, y + 1]
  guard $ x' >= 0 && y' >= 0
  guard $ (x' == x) /= (y' == y)
  let t = cave !! x' !! y'
  guard $ tool /= Neither || t /= Rocky
  guard $ tool /= Torch || t /= Wet
  guard $ tool /= Gear || t /= Narrow
  return ((x', y'), tool)

change :: [[Type]] -> State -> [State]
change cave ((x, y), tool) = do
  tool' <- [Torch, Gear, Neither]
  guard $ tool' /= tool
  let t = cave !! x !! y
  guard $ tool' /= Neither || t /= Rocky
  guard $ tool' /= Torch || t /= Wet
  guard $ tool' /= Gear || t /= Narrow
  return ((x, y), tool')

dijkstra :: [[Type]] -> State -> Set State -> Queue -> Int
dijkstra cave target visited (_0, _1, _2, _3, _4, _5, _6) =
  let unvisited = _0 \\ visited
      visited' = Set.union unvisited visited
      moves = Set.fromList $ concatMap (move cave) unvisited
      changes = Set.fromList $ concatMap (change cave) unvisited
  in if Set.member target unvisited
  then 0
  else 1 + dijkstra cave target visited' (Set.union _1 moves, _2, _3, _4, _5, _6, changes)

fastest :: (Int, Location) -> Int
fastest (depth, (x, y)) =
  let cave = types (depth, (x, y))
  in dijkstra cave ((x, y), Torch) Set.empty (Set.singleton ((0, 0), Torch), Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)

part1 :: String -> Int
part1 = risk . parse

part2 :: String -> Int
part2 = fastest . parse

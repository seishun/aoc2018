import Control.Monad
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord

type Coordinate = (Int, Int)
type Square = (Coordinate, Int)

power :: Int -> Coordinate -> Int
power serial (x, y)  =
  let rackID = x + 10
  in (rackID * y + serial) * rackID `quot` 100 `mod` 10 - 5

sat :: Int -> Map Coordinate Int
sat serial = m
  where m = Map.fromList $ do
          x <- [0..300]
          y <- [0..300]
          let val = case (x, y) of
                (0, _) -> 0
                (_, 0) -> 0
                (x, y) -> power serial (x, y) + m ! (x, y-1) + m ! (x-1, y) - m ! (x-1, y-1)
          return ((x, y), val)

totalPower :: Map Coordinate Int -> Square -> Int
totalPower m ((left, top), size) =
  let (right, bottom) = (left + size - 1, top + size - 1)
  in m ! (right, bottom) + m ! (left - 1, top - 1) - m ! (right, top - 1) - m ! (left - 1, bottom)

part1 :: Int -> Coordinate
part1 serial = fst $ maximumBy (comparing $ totalPower m) $ do
    x <- [1..298]
    y <- [1..298]
    return ((x, y), 3)
  where m = sat serial

part2 :: Int -> Square
-- maximumBy is shitty so I have to implement it myself to avoid a stack overflow
part2 serial = foldl1' cmp $ do
    x <- [1..300]
    y <- [1..300]
    size <- [1..300]
    guard $ x + size - 1 <= 300
    guard $ y + size - 1 <= 300
    return ((x, y), size)
  where m = sat serial
        cmp x1 x2
          | totalPower m x1 > totalPower m x2 = x1
          | otherwise = x2

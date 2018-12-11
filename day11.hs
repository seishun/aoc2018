import Data.List
import Data.Ord

type Coordinate = (Int, Int)

power :: Int -> Coordinate -> Int
power serial (x, y)  =
  let rackID = x + 10
  in (rackID * y + serial) * rackID `quot` 100 `mod` 10 - 5

totalPower :: Int -> Coordinate -> Int
totalPower serial (x, y) = sum $ do
  x' <- [x..x+2]
  y' <- [y..y+2]
  return $ power serial (x', y')

part1 :: Int -> Coordinate
part1 serial = maximumBy (comparing $ totalPower serial) $ do
  x <- [1..298]
  y <- [1..298]
  return (x, y)

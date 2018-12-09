import Control.Monad
import Data.List
import Data.Ord

type Coordinate = (Int, Int)

distance :: Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

closest :: Coordinate -> [Coordinate] -> Maybe Coordinate
closest location coordinates =
  let ((c,d1):(_,d2):_) = sortBy (comparing snd) $ zip coordinates (map (distance location) coordinates)
  in if d1 == d2
  then Nothing
  else Just c

parse :: String -> Coordinate
parse coordinate = (x, y)
  where [(x, ',':' ':y_)] = reads coordinate
        y = read y_

part1 :: String -> Int
part1 input =
  let coordinates = map parse $ lines $ input
      left = minimum $ map fst $ coordinates 
      right = maximum $ map fst $ coordinates
      top = minimum $ map snd $ coordinates
      bottom = maximum $ map snd $ coordinates
      infinites = nub $ do
        x <- [left .. right]
        y <- [top .. bottom]
        guard $ x `elem` [left, right] || y `elem` [top, bottom]
        Just coordinate <- [closest (x, y) coordinates]
        return coordinate
  in maximum $ map length $ group $ sort $ do
    x <- [left + 1 .. right - 1]
    y <- [top + 1 .. bottom - 1]
    Just coordinate <- [closest (x, y) coordinates]
    guard $ coordinate `notElem` infinites
    return coordinate

part2 :: String -> Int
part2 input =
  let coordinates = map parse $ lines $ input
      left = minimum $ map fst $ coordinates
      right = maximum $ map fst $ coordinates
      top = minimum $ map snd $ coordinates
      bottom = maximum $ map snd $ coordinates
  in length $ do
    x <- [left .. right]
    y <- [top .. bottom]
    guard $ sum (map (distance (x, y)) coordinates) < 10000
    return (x, y)

import Data.Set (Set)
import qualified Data.Set as Set

type Location = (Int, Int)

parse :: String -> Set Location
parse ('x':'=':xy_) =
  let [(x, ',':' ':'y':'=':y_)] = reads xy_
      [(y1, '.':'.':y2_)] = reads y_
      y2 = read y2_
  in Set.fromList [(y, x) | y <- [y1..y2]]
parse ('y':'=':yx_) =
  let [(y, ',':' ':'x':'=':x_)] = reads yx_
      [(x1, '.':'.':x2_)] = reads x_
      x2 = read x2_
  in Set.fromList [(y, x) | x <- [x1..x2]]

down :: Set Location -> Set Location -> Set Location -> Location -> (Set Location, Set Location, Bool)
down clay settled visited (y, x)
  | y == (fst $ maximum clay) = (Set.singleton (y, x), Set.empty, False)
  | (y + 1, x) `elem` visited = (Set.singleton (y, x), Set.empty, (y + 1, x) `elem` settled)
  | (y + 1, x) `elem` clay || (y + 1, x) `elem` settled =
    let (waterLeft, settledLeft, overflowLeft) = left clay settled visited (y, x - 1)
        settled' = Set.union settledLeft settled
        visited' = Set.union waterLeft visited
        (waterRight, settledRight, overflowRight) = right clay settled' visited' (y, x + 1)
        visited'' = Set.insert (y, x) $ Set.union waterLeft waterRight
        settled'' = if overflowLeft && overflowRight then visited'' else Set.union settledRight settled'
    in (visited'', settled'', overflowLeft && overflowRight)
  | otherwise =
    let (waterDown, settledDown, overflowDown) = down clay settled visited (y + 1, x)
    in if overflowDown
    then let (waterLeft, settledLeft, overflowLeft) = left clay (Set.union settledDown settled) visited (y, x - 1)
             settled' = Set.unions [settledDown, settledLeft, settled]
             visited' = Set.union waterLeft visited
             (waterRight, settledRight, overflowRight) = right clay settled' visited' (y, x + 1)
             visited'' = Set.insert (y, x) $ Set.unions [waterDown, waterLeft, waterRight]
             settled'' = if overflowLeft && overflowRight then visited'' else Set.union settledRight settled'
    in (visited'', settled'', overflowLeft && overflowRight)
    else (Set.insert (y, x) waterDown, settledDown, False)

left :: Set Location -> Set Location -> Set Location -> Location -> (Set Location, Set Location, Bool)
left = side (-1)

right :: Set Location -> Set Location -> Set Location -> Location -> (Set Location, Set Location, Bool)
right = side 1

side :: Int -> Set Location -> Set Location -> Set Location -> Location -> (Set Location, Set Location, Bool)
side direction clay settled visited (y, x)
  | (y, x) `elem` visited = (Set.empty, Set.empty, (y, x) `elem` settled)
  | (y, x) `elem` clay = (Set.empty, Set.empty, True)
  | (y + 1, x) `notElem` clay && (y + 1, x) `notElem` settled =
    let (waterDown, settledDown, overflowDown) = down clay settled visited (y + 1, x)
        (waterSide, settledSide, overflowSide) = side direction clay (Set.union waterDown settled) (Set.union waterDown visited) (y, x + direction)
    in if overflowDown
    then (Set.insert (y, x) $ Set.union waterDown waterSide, Set.insert (y, x) $ Set.union settledDown settledSide, overflowDown && overflowSide)
    else (Set.insert (y, x) waterDown, settledDown, False)
  | otherwise =
    let (waterSide, settledSide, overflowSide) = side direction clay settled visited (y, x + direction)
    in (Set.insert (y, x) waterSide, settledSide, overflowSide)

part1 :: String -> Int
part1 input =
  let clay = Set.unions $ map parse $ lines input
      top = fst $ minimum clay
      (visited, _, _) = down clay Set.empty Set.empty (0, 500)
  in Set.size $ Set.filter ((>= top) . fst) $ visited

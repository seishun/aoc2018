import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map 
import Data.Set (Set)
import qualified Data.Set as Set

type Location = (Int, Int)
type Adjacency = (Location, [Location])
type Adjacencies = Map Location [Location]

adjacencies :: String -> Location -> (Set Location, Set Adjacency, String, Bool)
adjacencies (c:cs) (y,x) = case c of
  '^' -> adjacencies cs (y,x) 
  '$' -> (Set.singleton (y,x), Set.empty, cs, undefined)
  ')' -> (Set.singleton (y,x), Set.empty, cs, True)
  '|' -> (Set.singleton (y,x), Set.empty, cs, False)
  '(' ->
    let (locations, adjacencies', cs') = untilEnd cs
        (locations', adjacencies'', (cs'':_), (end:_)) = unzip4 $ map (adjacencies cs') $ Set.toList locations
    in (Set.unions locations', Set.unions (adjacencies':adjacencies''), cs'', end)
    where untilEnd cs =
            let (locations, adjacencies', cs', end) = adjacencies cs (y,x)
            in if end
            then (locations, adjacencies', cs')
            else let (locations', adjacencies'', cs'') = untilEnd cs'
            in (Set.union locations locations', Set.union adjacencies' adjacencies'', cs'')
  _ ->
    let (y',x') = case c of
          'N' -> (y-1,x)
          'S' -> (y+1,x)
          'E' -> (y,x+1)
          'W' -> (y,x-1)
        (locations, adjacencies', cs', end) = adjacencies cs (y',x')
    in (locations, Set.insert ((y,x),[(y',x')]) $ Set.insert ((y',x'),[(y,x)]) adjacencies', cs', end)

bfs :: Set Location -> Set Location -> Adjacencies -> Int
bfs visited unvisited adjacencies =
  let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (adjacencies !) unvisited
      visited' = Set.union visited unvisited
  in if Set.null unvisited'
  then 0
  else 1 + bfs visited' unvisited' adjacencies

part1 :: String -> Int
part1 input =
  let (_, adjacencies', _, _) = adjacencies input (0,0)
  in bfs Set.empty (Set.singleton (0,0)) $ Map.fromListWith (++) $ Set.toList adjacencies'

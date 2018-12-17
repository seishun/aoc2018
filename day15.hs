import Control.Monad
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data Class = Elf | Goblin deriving (Eq, Show)
type Unit = (Class, Int)
type Location = (Int, Int)
type State = Map Location Unit

parse' :: [(Int, Char)] -> (String, [(Int, Unit)])
parse' ((i,l):xs) =
  let (s, us) = parse' xs
  in case l of
    'E' -> ('.':s, (i, (Elf, 200)):us)
    'G' -> ('.':s, (i, (Goblin, 200)):us)
    _ -> (l:s, us)
parse' [] = ("", [])

parse :: String -> ([String], State)
parse input =
  let (ls, us) = unzip . map p . zip [0..] . map (parse' . zip [0..]) . lines $ input
  in (ls, Map.fromList $ concat us)
  where p (y, (l, us)) = (l, map (\(x, u) -> ((y, x), u)) us)

adjacent :: [String] -> State -> Location -> [Location]
adjacent cave state (y, x) = do
  (y', x') <- [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
  guard $ y' >= 0 && y' < length cave
  guard $ x' >= 0 && x' < length (cave !! y')
  guard $ cave !! y' !! x' == '.'
  guard $ Map.notMember (y', x') state
  return (y', x')

bfs :: [String] -> State -> [Location] -> Set Location -> Set Location -> Maybe Int
bfs cave state to visited unvisited =
  if any (`elem` to) unvisited
  then Just 0
  else let unvisited' = Set.filter (`Set.notMember` visited) $ Set.fromList $ concatMap (adjacent cave state) unvisited
           visited' = Set.union visited unvisited
  in if Set.null unvisited'
  then Nothing
  else (1 +) <$> bfs cave state to visited' unvisited'

move :: [String] -> [Location] -> (State, Location) -> (State, Location)
move cave targets (state, (y, x)) =
  if any (`elem` [(y+1,x), (y-1,x), (y,x+1), (y,x-1)]) targets
  then (state, (y, x))
  else let inRange = nub $ concatMap (adjacent cave state) targets
           steps = sort $ do
              (y', x') <- adjacent cave state (y, x)
              distance <- maybeToList $ bfs cave state inRange Set.empty $ Set.singleton (y', x')
              return (distance, (y', x'))
  in case listToMaybe steps of
    Nothing -> (state, (y, x))
    Just (_, (y', x')) ->
      let (Just unit, state') = Map.updateLookupWithKey (\_ _ -> Nothing) (y, x) state
      in (Map.insert (y', x') unit state', (y', x'))

attack :: Int -> [String] -> [Location] -> (State, Location) -> State
attack power cave targets (state, (y, x)) =
  case sort $ catMaybes $ map inRange targets of
    [] -> state
    ((hp, target):_) ->
      let hp' = hp - power
      in if hp' <= 0
      then Map.delete target state
      else Map.adjust (\(c, _) -> (c, hp')) target state
  where inRange (y', x') =
          if (y', x') `elem` [(y+1,x), (y-1,x), (y,x+1), (y,x-1)]
          then Just (snd $ state Map.! (y', x'), (y', x'))
          else Nothing

next' :: [String] -> State -> Int -> [(Location, Unit)] -> (State, Bool)
next' cave state elfPower (((y, x), unit@(c, _)):xs) =
       let targets = Map.keys $ Map.filter ((/= c) . fst) state
       in if null targets
       then (state, True)
       else let power = case c of
                  Elf -> elfPower
                  Goblin -> 3
                state' = attack power cave targets $ move cave targets (state, (y, x))
                xs' = filter ((`Map.member` state') . fst) xs
            in next' cave state' elfPower xs'
next' _ state _ [] = (state, False)

next :: [String] -> State -> Int -> (State, Bool)
next cave state elfPower = next' cave state elfPower $ Map.toList state

outcome :: Int -> [String] -> State -> Int
outcome rounds cave state =
  let (state', end) = next cave state 3
  in if end
  then rounds * sum (map snd $ Map.elems state')
  else outcome (rounds + 1) cave state'

goodOutcome :: Int -> [String] -> State -> Int -> Maybe Int
goodOutcome rounds cave state elfPower =
  let (state', end) = next cave state elfPower
  in if ((/=) `on` (Map.size . Map.filter ((== Elf) . fst))) state state'
  then Nothing
  else if end
  then Just $ rounds * sum (map snd $ Map.elems state')
  else goodOutcome (rounds + 1) cave state' elfPower

part1 :: String -> Int
part1 input =
  let (cave, state) = parse input
  in outcome 0 cave state

part2 :: String -> Int
part2 input =
  let (cave, state) = parse input
  in head $ catMaybes $ map (goodOutcome 0 cave state) [4..]

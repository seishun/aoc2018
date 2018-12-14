import Data.Complex
import Data.Map (Map)
import qualified Data.Map as Map

type Direction = Complex Float
type Turn = Complex Float

type Location = (Int, Int)
type Cart = (Direction, [Turn])
type State = Map Location Cart

parse' :: [(Int, Char)] -> (String, [(Int, Cart)])
parse' ((i,l):xs) =
  let (s, cs) = parse' xs
  in case l of
    '^' -> ('|':s, (i, (((-1):+   0 ), turns)):cs)
    'v' -> ('|':s, (i, ((  1 :+   0 ), turns)):cs)
    '<' -> ('-':s, (i, ((  0 :+ (-1)), turns)):cs)
    '>' -> ('-':s, (i, ((  0 :+   1 ), turns)):cs)
    _ -> (l:s, cs)
  where turns = cycle [(0 :+ 1), (1 :+ 0), (0 :+ (-1))]
parse' [] = ("", [])

parse :: String -> ([String], State)
parse input =
  let (ls, cs) = unzip . map p . zip [0..] . map (parse' . zip [0..]) . lines $ input
  in (ls, Map.fromList $ concat cs)
  where p (y, (l, cs)) = (l, map (\(x, c) -> ((y, x), c)) cs)

turn :: Cart -> Char -> Cart
turn (d, (t:xs)) '+' = (d * t, xs)
turn ((y :+ x), turns) '/'  = (((-x) :+ (-y)), turns)
turn ((y :+ x), turns) '\\' = ((  x  :+   y ), turns)
turn (c, turns) '|' = (c, turns)
turn (c, turns) '-' = (c, turns)

tick' :: [String] -> State -> [(Location, Cart)] -> Either State Location
tick' tracks state (((y, x), cart@((vy :+ vx), turns)):xs) =
  let (y', x') = (y + round vy, x + round vx)
  in if Map.member (y', x') state
  then Right (y', x')
  else
    let cart' = turn cart $ tracks !! y' !! x'
        state' = Map.insert (y', x') cart' $ Map.delete (y, x) state
    in tick' tracks state' xs
tick' _ state [] = Left state

tick :: [String] -> State -> Either State Location
tick tracks state = tick' tracks state $ Map.toList state

firstCollision :: [String] -> State -> Location
firstCollision tracks state = case tick tracks state of
  Left state' -> firstCollision tracks state'
  Right location -> location

part1 :: String -> (Int, Int)
part1 input =
  let (tracks, state) = parse input
      (y, x) = firstCollision tracks state
  in (x, y)

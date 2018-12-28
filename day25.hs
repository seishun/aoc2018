import Data.List

type Point = [Int]

parse :: String -> Point
parse line = read $ "[" ++ line ++ "]"

distance :: Point -> Point -> Int
distance a b = sum $ map abs $ zipWith subtract a b

constellate' :: [Point] -> [Point] -> [Point]
constellate' constellation points =
  case partition (\p -> any ((<=3) . distance p) constellation) points of
    ([], _) -> points
    (constellation', points') -> constellate' constellation' points'

constellate :: [Point] -> Int
constellate points = case points of
  [] -> 0
  (point:xs) ->
    let points' = constellate' [point] xs
    in 1 + constellate points'

part1 :: String -> Int
part1 = constellate . map parse . lines

import Data.List
import Data.Ord

type Position = (Int, Int, Int)
type Nanobot = (Position, Int)

parse :: String -> Nanobot
parse ('p':'o':'s':'=':'<':x_) =
  let [(x, ',':y_)] = reads x_
      [(y, ',':z_)] = reads y_
      [(z, '>':',':' ':'r':'=':r_)] = reads z_
      r = read r_
  in ((x, y, z), r)

toRange :: Position -> Nanobot -> Int
toRange (x, y, z) ((x', y', z'), radius) =
  let distance = abs (x - x') + abs (y - y') + abs (z - z')
  in distance - radius

cluster :: [Nanobot] -> [Nanobot]
cluster (bot@(pos, r) : xs) = bot : cluster (filter ((<= r) . toRange pos) xs)
cluster [] = []

part1 :: String -> Int
part1 input =
  let nanobots = map parse $ lines input
      strongest = maximumBy (comparing snd) nanobots
  in length $ filter ((<= 0) . (`toRange` strongest) . fst) nanobots

part2 :: String -> Int
part2 input =
  let nanobots = sortOn (negate . snd) $ map parse $ lines input
  in maximum $ map (toRange (0,0,0)) $ cluster nanobots

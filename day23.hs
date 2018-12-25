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

inRange :: Position -> Nanobot -> Bool
inRange (x, y, z) ((x', y', z'), radius) =
  let distance = abs (x - x') + abs (y - y') + abs (z - z')
  in radius >= distance

part1 :: String -> Int
part1 input =
  let nanobots = map parse $ lines input
      strongest = maximumBy (comparing snd) nanobots
  in length $ filter ((`inRange` strongest) . fst) nanobots

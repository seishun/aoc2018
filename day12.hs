import Data.Map (Map, (!))
import qualified Data.Map as Map

type Notes = Map String Char

parse' :: [String] -> (String, Char)
parse' [from, "=>", [to]] = (from, to)

parse :: [String] -> (String, Notes)
parse (first : "" : xs) =
  let ["initial", "state:", initial] = words first
  in (initial, Map.fromList $ map (parse' . words) xs)

next' :: Notes -> String -> String
next' notes (p1:p2:p3:p4:p5:xs) = notes ! [p1,p2,p3,p4,p5] : next' notes (p2:p3:p4:p5:xs)
next' notes [p1, p2, p3, p4] = notes ! [p1,p2,p3,p4,'.'] : next' notes [p2,p3,p4]
next' notes [p1, p2, p3] = notes ! [p1,p2,p3,'.','.'] : next' notes [p2,p3]
next' notes [p1, p2] = [notes ! [p1,p2,'.','.','.']]

next :: Notes -> (String, Int) -> (String, Int)
-- start with at least 3 dots
next notes (state@('.':'.':'.':xs), offset) = (next' notes state, offset + 2)
next notes (state, offset) = next notes (('.':state), offset - 1)

nextN :: Notes -> (String, Int) -> Int -> (String, Int)
nextN notes (state, offset) n =
  let (state', offset') = next notes (state, offset)
  in if state == state'
  then (state, offset + (offset' - offset) * n)
  else nextN notes (state', offset') (n - 1)

part1 :: String -> Int
part1 input =
  let (initial, notes) = parse $ lines input
      (final, offset) = iterate (next notes) (initial, 0) !! 20
  in sum $ map fst $ filter ((== '#') . snd) $ zip [offset..] final

part2 :: String -> Int
part2 input =
  let (initial, notes) = parse $ lines input
      (final, offset) = nextN notes (initial, 0) 50000000000
  in sum $ map fst $ filter ((== '#') . snd) $ zip [offset..] final

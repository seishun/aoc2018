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

part1 :: String -> Int
part1 input =
  let (initial, notes) = parse $ lines input
      (final, offset) = iterate (next notes) (initial, 0) !! 20
  in sum $ map fst $ filter ((== '#') . snd) $ zip [offset..] final

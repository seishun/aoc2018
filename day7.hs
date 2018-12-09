import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

parse :: [String] -> (Char, [Char])
parse ["Step", [req], "must", "be", "finished", "before", "step", [step], "can", "begin."] = (step, [req])

next :: [Char] -> [Char] -> Map Char [Char] -> Maybe Char
next completed scheduled requirements = find available ['A'..'Z']
  where available step = step `notElem` (completed ++ scheduled) && all (`elem` completed) (Map.findWithDefault [] step requirements)

order :: String -> Map Char [Char] -> String
order completed requirements = case next completed [] requirements of
  Just step -> order (completed ++ [step]) requirements
  Nothing -> completed

duration :: Char -> Int
duration step = 60 + ord step - ord 'A' + 1

time :: [Char] -> Int -> Int -> Map Int Char -> Map Char [Char] -> Int
time completed elapsed workers scheduled requirements =
  case (workers, next completed (Map.elems scheduled) requirements) of
    (0, _) -> let ((elapsed', step), scheduled') = Map.deleteFindMin scheduled
      in time (step:completed) elapsed' 1 scheduled' requirements
    (_, Nothing) -> case Map.minViewWithKey scheduled of
      Just ((elapsed', step), scheduled') -> time (step:completed) elapsed' (workers + 1) scheduled' requirements
      Nothing -> elapsed
    (_, Just step) -> let scheduled' = Map.insert (elapsed + duration step) step scheduled
      in time completed elapsed (workers - 1) scheduled' requirements

part1 :: String -> String
part1 = order "" . Map.fromListWith (++) . map (parse . words) . lines

part2 :: String -> Int
part2 = time [] 0 5 Map.empty . Map.fromListWith (++) . map (parse . words) . lines

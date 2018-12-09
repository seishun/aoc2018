import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

parse :: [String] -> (Char, [Char])
parse ["Step", [req], "must", "be", "finished", "before", "step", [step], "can", "begin."] = (step, [req])

next :: [Char] -> Map Char [Char] -> Maybe Char
next completed requirements = find available ['A'..'Z']
  where available step = step `notElem` completed && all (`elem` completed) (Map.findWithDefault [] step requirements)

order :: String -> Map Char [Char] -> String
order completed requirements = case next completed requirements of
  Just step -> order (completed ++ [step]) requirements
  Nothing -> completed

part1 :: String -> String
part1 = order "" . Map.fromListWith (++) . map (parse . words) . lines

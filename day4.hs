import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

data Event = Begin Int | Fall | Wake
type DateTime = (Int, Int, Int, Int)
type Record = (DateTime, Event)

parseDateTime :: String -> String -> (Int, Int, Int, Int)
parseDateTime ('[':'1':'5':'1':'8':'-':date_) time_ = (month, read day_, hour, minute)
  where [(month, '-':day_)] = reads date_
        [(hour, ':':minute_)] = reads time_
        [(minute, "]")] = reads minute_

parseRecord :: [String] -> Record
parseRecord [date_, time_, "Guard", '#':id_, "begins", "shift"] = (parseDateTime date_ time_, Begin (read id_))
parseRecord [date_, time_, "falls", "asleep"] = (parseDateTime date_ time_, Fall)
parseRecord [date_, time_, "wakes", "up"] = (parseDateTime date_ time_, Wake)

process :: Int -> [Record] -> Map Int (Map Int Int)
process _ ((_, Begin id) : xs) = process id xs
process id (((_, _, _, start), Fall) : ((_, _, _, end), Wake) : xs) = Map.insertWith (Map.unionWith (+)) id minutes (process id xs)
  where minutes = Map.fromList $ do
        minute <- [start .. end - 1]
        return (minute, 1)
process _ [] = Map.empty

parseInput :: String -> Map Int (Map Int Int)
parseInput = process 0 . sortBy (comparing fst) . map (parseRecord . words) . lines

mostAsleepGuard :: Map Int (Map Int Int) -> (Int, Map Int Int)
mostAsleepGuard = maximumBy (comparing (Map.foldr (+) 0 . snd)) . Map.toList

mostAsleepMinute :: Map Int Int -> (Int, Int)
mostAsleepMinute = maximumBy (comparing snd) . Map.toList

part1 :: String -> Int
part1 input = guard * minute
  where (guard, minutes) = mostAsleepGuard $ parseInput input
        minute = fst $ mostAsleepMinute minutes

part2 :: String -> Int
part2 input = guard * minute
  where (guard, (minute, _)) = maximumBy (comparing (fst . snd)) $ Map.toList $ Map.map mostAsleepMinute $ parseInput input

import Data.Map (Map, (!))
import qualified Data.Map as Map 

type Location = (Int, Int)

parse :: String -> (Int, Location)
parse input =
  let ["depth:", depth_, "target:", target_] = words input
  in (read depth_, read $ "(" ++ target_ ++ ")")

risk :: (Int, Location) -> Int
risk (depth, (x, y)) = Map.foldr ((+) . (`mod` 3)) 0 levels
  where levels = Map.fromList $ do
        x' <- [0..x]
        y' <- [0..y]
        let index = case (x', y') of
              (0, 0) -> 0
              (_, 0) -> x' * 16807
              (0, _) -> y' * 48271
              (_, _) ->
                if (x', y') == (x, y)
                then 0
                else levels ! (x'-1, y') * levels ! (x', y'-1)
        return ((x', y'), (index + depth) `mod` 20183)

part1 :: String -> Int
part1 = risk . parse

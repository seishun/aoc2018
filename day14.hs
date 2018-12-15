import Data.Sequence
import Prelude hiding (length, take, drop)

type State = (Seq Int, Int, Int)

next :: State -> State
next (recipes, first, second) =
  let firstScore = recipes `index` first
      secondScore = recipes `index` second
      sum = firstScore + secondScore
      recipes' = case sum `div` 10 of
        0 -> recipes :|> sum
        tens -> recipes :|> tens :|> sum `mod` 10
      first' = (first + firstScore + 1) `mod` length recipes'
      second' = (second + secondScore + 1) `mod` length recipes' 
  in (recipes', first', second')

nextUntil :: State -> Int -> Seq Int
nextUntil state number =
  let state'@(recipes, _, _) = next state
  in if length recipes >= number + 10
  then take 10 $ drop number recipes
  else nextUntil state' number

part1 :: Int -> String
part1 = concat . fmap show . nextUntil (fromList [3,7], 0, 1)

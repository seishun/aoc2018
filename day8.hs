import Control.Monad.State

metadataSum :: [Int] -> (Int, [Int])
metadataSum (qc : qm : xs) =
  let (cm, xs') = runState (replicateM qc $ state metadataSum) xs
  in (sum cm + sum (take qm xs'), drop qm xs')

value :: [Int] -> (Int, [Int])
value (qc : qm : xs) =
  let (cv, xs') = runState (replicateM qc $ state value) xs
      m = take qm xs'
      xs'' = drop qm xs'
      ref entry
        | entry == 0 || entry > length cv = 0
        | otherwise = cv !! (entry - 1)
  in if qc == 0
  then (sum m, xs'')
  else (sum $ map ref m, xs'')

part1 :: String -> Int
part1 = fst . metadataSum . map read . words

part2 :: String -> Int
part2 = fst . value . map read. words

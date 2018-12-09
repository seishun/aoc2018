import Control.Monad.State

metadataSum :: [Int] -> (Int, [Int])
metadataSum (qc : qm : xs) =
  let (cm, xs') = runState (replicateM qc $ state metadataSum) xs
  in (sum cm + sum (take qm xs'), drop qm xs')

part1 :: String -> Int
part1 = fst . metadataSum . map read . words

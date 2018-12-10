type Position = (Int, Int)
type Velocity = (Int, Int)
type Point = (Position, Velocity)

parse :: String -> Point
parse ('p':'o':'s':'i':'t':'i':'o':'n':'=':'<':xs) =
  let [(px, (',':' ':xs'))] = reads xs
      [(py, ('>':' ':'v':'e':'l':'o':'c':'i':'t':'y':'=':'<':xs''))] = reads xs'
      [(vx, (',':' ':vy_))] = reads xs''
      [(vy, ">")] = reads vy_
  in ((px, py), (vx, vy))

borders :: [Position] -> (Int, Int, Int, Int)
borders positions =
  let left = minimum $ map fst $ positions
      right = maximum $ map fst $ positions
      top = minimum $ map snd $ positions
      bottom = maximum $ map snd $ positions
  in (left, right, top, bottom)

draw :: [Point] -> String
draw points =
  let positions = map fst points
      (left, right, top, bottom) = borders positions
  in unlines $ flip map [top..bottom] $ \y -> do
    x <- [left..right]
    if (x, y) `elem` positions
    then return '#'
    else return '.'

move :: [Point] -> [Point]
move = map add
  where add ((px, py), (vx, vy)) = ((px + vx, py + vy), (vx, vy))

part1 :: String -> IO ()
part1 = mapM_ (\s -> putStrLn s >> getLine) . map draw . filter sane . iterate move . map parse . lines
  where sane points =
          let (left, right, top, bottom) = borders $ map fst points
          in right - left + bottom - top < 100

part2 :: String -> IO ()
part2 = mapM_ (\(t, s) -> print t >> putStrLn (draw s) >> getLine) . filter sane . zip [0..] . iterate move . map parse . lines
  where sane (_, points) =
          let (left, right, top, bottom) = borders $ map fst points
          in right - left + bottom - top < 100

parse :: String -> Int
parse ('+' : xs) = read xs
parse xs = read xs

part1 :: String -> Int
part1 = sum . map parse . lines

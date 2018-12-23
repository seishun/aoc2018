import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set

type Registers = [Int]
type Op = Registers -> Int -> Int -> Int -> Registers
type Instruction = (Op, Int, Int, Int)

get :: Int -> Registers -> Int
get 0 [value, _, _, _, _, _] = value
get 1 [_, value, _, _, _, _] = value
get 2 [_, _, value, _, _, _] = value
get 3 [_, _, _, value, _, _] = value
get 4 [_, _, _, _, value, _] = value
get 5 [_, _, _, _, _, value] = value

set :: Int -> Int -> Registers -> Registers
set 0 value [_, _1, _2, _3, _4, _5] = [value, _1, _2, _3, _4, _5]
set 1 value [_0, _, _2, _3, _4, _5] = [_0, value, _2, _3, _4, _5]
set 2 value [_0, _1, _, _3, _4, _5] = [_0, _1, value, _3, _4, _5]
set 3 value [_0, _1, _2, _, _4, _5] = [_0, _1, _2, value, _4, _5]
set 4 value [_0, _1, _2, _3, _, _5] = [_0, _1, _2, _3, value, _5]
set 5 value [_0, _1, _2, _3, _4, _] = [_0, _1, _2, _3, _4, value]

op :: (Int -> Registers -> Int) -> (Int -> Registers -> Int) -> (Int -> Int -> Int) -> Op
op getA getB fun registers a b c = set c (fun (getA a registers) (getB b registers)) registers

parseOpcode :: String -> Op
parseOpcode "addr" = op get get (+)
parseOpcode "addi" = op get (const . id) (+)
parseOpcode "mulr" = op get get (*)
parseOpcode "muli" = op get (const . id) (*)
parseOpcode "banr" = op get get (.&.)
parseOpcode "bani" = op get (const . id) (.&.)
parseOpcode "borr" = op get get (.|.)
parseOpcode "bori" = op get (const . id) (.|.)
parseOpcode "setr" = op get get const
parseOpcode "seti" = op (const . id) get const
parseOpcode "gtir" = op (const . id) get (\x y -> fromEnum $ x > y)
parseOpcode "gtri" = op get (const . id) (\x y -> fromEnum $ x > y)
parseOpcode "gtrr" = op get get (\x y -> fromEnum $ x > y)
parseOpcode "eqir" = op (const . id) get (\x y -> fromEnum $ x == y)
parseOpcode "eqri" = op get (const . id) (\x y -> fromEnum $ x == y)
parseOpcode "eqrr" = op get get (\x y -> fromEnum $ x == y)

parseInstruction :: String -> Instruction
parseInstruction instruction =
  let [op, a, b, c] = words instruction
  in (parseOpcode op, read a, read b, read c)

parse :: [String] -> (Int, [Instruction])
parse (('#':'i':'p':' ':ip_):xs) = (read ip_, map parseInstruction xs)

next :: Int -> Int -> Int
next value seed =
  let quotients = map (.&. 255) $ takeWhile (> 0) $ iterate (`div` 256) $ value .|. 65536
  in foldl proc seed quotients
  where proc cur = (.&. 16777215) . (* 65899) . (.&. 16777215) . (+ cur)

highest :: Set Int -> Int -> Int -> Int
highest seen value seed =
  let value' = next value seed
  in if value' `elem` seen
  then value
  else highest (Set.insert value' seen) value' seed

run :: Registers -> (Int, [Instruction]) -> Int
run registers (ip, instructions) =
  let ip' = get ip registers
      (op, a, b, c) = instructions !! ip'
      registers' = op registers a b c
  in if ip' == 7 then a
  else run (set ip (get ip registers' + 1) registers') (ip, instructions)

part1 :: String -> Int
part1 = next 0 . run [0,0,0,0,0,0] . parse . lines

part2 :: String -> Int
part2 = highest Set.empty 0 . run [0,0,0,0,0,0] . parse . lines

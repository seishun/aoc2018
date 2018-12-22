import Data.Bits

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

set :: Int -> Registers -> Int -> Registers
set 0 [_, _1, _2, _3, _4, _5] value = [value, _1, _2, _3, _4, _5]
set 1 [_0, _, _2, _3, _4, _5] value = [_0, value, _2, _3, _4, _5]
set 2 [_0, _1, _, _3, _4, _5] value = [_0, _1, value, _3, _4, _5]
set 3 [_0, _1, _2, _, _4, _5] value = [_0, _1, _2, value, _4, _5]
set 4 [_0, _1, _2, _3, _, _5] value = [_0, _1, _2, _3, value, _5]
set 5 [_0, _1, _2, _3, _4, _] value = [_0, _1, _2, _3, _4, value]

op :: (Int -> Registers -> Int) -> (Int -> Registers -> Int) -> (Int -> Int -> Int) -> Op
op getA getB fun registers a b c = set c registers $ fun (getA a registers) (getB b registers)

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

run :: Registers -> (Int, [Instruction]) -> Registers
run registers (ip, instructions) =
  let ip' = get ip registers
  in if ip' >= length instructions || ip' < 0
  then registers
  else let (op, a, b, c) = instructions !! ip'
           registers' = op registers a b c
  in run (set ip registers' $ get ip registers' + 1) (ip, instructions)

part1 :: String -> Int
part1 = get 0 . run [0,0,0,0,0,0] . parse . lines

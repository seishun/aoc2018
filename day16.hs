import Data.Bits
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple

type Registers = [Int]
type Op = Registers -> Int -> Int -> Int -> Registers
type Instruction = (Int, Int, Int, Int)

get :: Int -> Registers -> Int
get 0 [value, _, _, _] = value
get 1 [_, value, _, _] = value
get 2 [_, _, value, _] = value
get 3 [_, _, _, value] = value

set :: Int -> Registers -> Int -> Registers
set 0 [_, _1, _2, _3] value = [value, _1, _2, _3]
set 1 [_0, _, _2, _3] value = [_0, value, _2, _3]
set 2 [_0, _1, _, _3] value = [_0, _1, value, _3]
set 3 [_0, _1, _2, _] value = [_0, _1, _2, value]

op :: (Int -> Registers -> Int) -> (Int -> Registers -> Int) -> (Int -> Int -> Int) -> Op
op getA getB fun registers a b c = set c registers (fun (getA a registers) (getB b registers))

addr :: Op
addr = op get get (+)

addi :: Op
addi = op get (const . id) (+)

mulr :: Op
mulr = op get get (*)

muli :: Op
muli = op get (const . id) (*)

banr :: Op
banr = op get get (.&.)

bani :: Op
bani = op get (const . id) (.&.)

borr :: Op
borr = op get get (.|.)

bori :: Op
bori = op get (const . id) (.|.)

setr :: Op
setr = op get get const

seti :: Op
seti = op (const . id) get const

gtir :: Op
gtir = op (const . id) get (\x y -> fromEnum $ x > y)

gtri :: Op
gtri = op get (const . id) (\x y -> fromEnum $ x > y)

gtrr :: Op
gtrr = op get get (\x y -> fromEnum $ x > y)

eqir :: Op
eqir = op (const . id) get (\x y -> fromEnum $ x == y)

eqri :: Op
eqri = op get (const . id) (\x y -> fromEnum $ x == y)

eqrr :: Op
eqrr = op get get (\x y -> fromEnum $ x == y)

opcodes :: [Op]
opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

execute :: [Op] -> Registers -> Instruction -> Registers
execute ops registers (op, a, b, c) = (ops !! op) registers a b c

parseInstruction :: String -> Instruction
parseInstruction instruction =
  let [op, a, b, c] = map read $ words $ instruction
  in (op, a, b, c)

parse :: [String] -> ([(Registers, Instruction, Registers)], [Instruction])
parse ("" : "" : xs) = ([], map parseInstruction xs)
parse (('B':'e':'f':'o':'r':'e':':':' ':before) : instruction : ('A':'f':'t':'e':'r':':':' ':' ':after) : "" : xs) =
  let (samples, program) = parse xs
  in ((read before, parseInstruction instruction, read after) : samples, program)

ops :: [(Registers, Instruction, Registers)] -> Map Int Int -> [Op]
ops samples mapping =
  if Map.size mapping == length opcodes
  then map (\(_, i) -> opcodes !! i) $ sort $ map swap $ Map.toList mapping
  else ops samples $ foldl matching mapping samples
  where matching mapping (before, (op, a, b, c), after) =
          case filter (\(i, op) -> op before a b c == after && i `Map.notMember` mapping) $ zip [0..] opcodes of
            [(i, _)] -> Map.insert i op mapping
            _ -> mapping

part1 :: String -> Int
part1 = length . filter (>= 3) . map matching . fst . parse . lines
  where matching (before, (_, a, b, c), after) = length $ filter (\op -> op before a b c == after) opcodes

part2 :: String -> Int
part2 input =
  let (samples, program) = parse $ lines input
  in get 0 $ foldl (execute $ ops samples Map.empty) [0,0,0,0] program

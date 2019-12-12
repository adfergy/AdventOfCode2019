type Value = Int
type Memory = [Value]
type Location = Int
type Mode = Int
type Output = (Memory, [Value])
type Input = [Int]
type PhaseSetting = Int

data Opcode = Code Mode Mode Mode Operation
    deriving Show

data Operation = Add | Mult | Input | Output | JumpTrue | JumpFalse | LessThan | Equal | Halt 
    deriving Show

testInputA :: Memory
testInputA = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

testInputB :: Memory
testInputB = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

testInputC :: Memory
testInputC = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

input :: Memory
input = [3,8,1001,8,10,8,105,1,0,0,21,34,47,72,81,94,175,256,337,418,99999,3,9,102,3,9,9,1001,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,5,9,1001,9,2,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99]

lookupV :: Memory -> Location -> Value
lookupV mem n = mem!!n

write :: Memory -> Location -> Value -> Memory
write mem n v = (take n mem) ++ [v] ++ (drop (n+1) mem)

getOpcode :: Value -> Opcode
getOpcode n = let ds = digs n in 
              let vs = drop (length ds) (take 5 (repeat 0)) ++ ds in
              Code (vs!!0) (vs!!1) (vs!!2) (getOperation (vs!!3*10+vs!!4))

getOperation :: Value -> Operation
getOperation n = case n of 
                  1  -> Add 
                  2  -> Mult
                  3  -> Input
                  4  -> Output
                  5  -> JumpTrue
                  6  -> JumpFalse
                  7  -> LessThan
                  8  -> Equal
                  99 -> Halt
                  n  -> error ("*** unimplemented opcode " ++ (show n) ++ " ***")

getValue :: Memory -> Value -> Mode -> Value
getValue mem v m = if m==0 then lookupV mem v else v  

digs :: Int -> [Int]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

compute :: Memory -> Location -> Input -> Output
compute mem pointer inp = let vals = drop pointer mem in
                          let Code c b a op = getOpcode (head vals) in
                              case op of
                                  Add       -> compute (write mem (vals!!3) (getValue mem (vals!!1) a + getValue mem (vals!!2) b)) (pointer + 4) inp

                                  Mult      -> compute (write mem (vals!!3) (getValue mem (vals!!1) a * getValue mem (vals!!2) b)) (pointer + 4) inp

                                  Input     -> compute (write mem (vals!!1) (head inp)) (pointer + 2) (tail inp)

                                  Output    -> let (mem', out) = compute mem (pointer + 2) inp in
                                                      (mem', (getValue mem (vals!!1) a):out)

                                  JumpTrue  -> if (getValue mem (vals!!1) a) == 0 then 
                                                compute mem (pointer + 3) inp
                                                else compute mem (getValue mem (vals!!2) b) inp

                                  JumpFalse -> if (getValue mem (vals!!1) a) == 0 then 
                                                 compute mem (getValue mem (vals!!2) b) inp
                                                 else compute mem (pointer + 3) inp

                                  LessThan  -> if (getValue mem (vals!!1) a) < (getValue mem (vals!!2) b) then
                                                 compute (write mem (vals!!3) 1) (pointer + 4) inp
                                                 else compute (write mem (vals!!3) 0) (pointer + 4) inp
 
                                  Equal     -> if (getValue mem (vals!!1) a) == (getValue mem (vals!!2) b) then
                                                 compute (write mem (vals!!3) 1) (pointer + 4) inp
                                                 else compute (write mem (vals!!3) 0) (pointer + 4) inp

                                  Halt      -> (mem, [])

permutations :: [PhaseSetting] -> [[PhaseSetting]]
permutations [] = [[]]
permutations ps = concat (map (\p -> map (\ps -> p:ps) (permutations (filter (\x -> not (x==p)) ps))) ps)

amplifier :: PhaseSetting -> Value -> Memory -> Value
amplifier ps v mem = (head.snd) (compute mem 0 [ps, v])

calculateThrust :: [PhaseSetting] -> Memory -> Value
calculateThrust [ps1, ps2, ps3, ps4, ps5] mem = let out1 = amplifier ps1 0 mem in
                                                let out2 = amplifier ps2 out1 mem in 
                                                let out3 = amplifier ps3 out2 mem in 
                                                let out4 = amplifier ps4 out3 mem in 
                                                  amplifier ps5 out4 mem

answer :: Value
answer = maximum (map (\ps -> calculateThrust ps input) (permutations [0..4]))

-- answer = 17406

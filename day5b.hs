type Value = Int
type Memory = [Value]
type Location = Int
type Mode = Int
type Output = (Memory, [Value])

data Opcode = Code Mode Mode Mode Operation
    deriving Show

data Operation = Add | Mult | Input | Output | JumpTrue | JumpFalse | LessThan | Equal | Halt 
    deriving Show

input :: Memory
input = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,37,34,224,101,-71,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1002,113,50,224,1001,224,-2550,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,13,50,225,102,7,187,224,1001,224,-224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,79,72,225,1101,42,42,225,1102,46,76,224,101,-3496,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,51,90,225,1101,11,91,225,1001,118,49,224,1001,224,-140,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,191,87,224,1001,224,-1218,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1,217,83,224,1001,224,-124,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,32,77,225,1101,29,80,225,101,93,58,224,1001,224,-143,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1101,45,69,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,374,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1006,224,434,1001,223,1,223,107,226,677,224,102,2,223,223,1005,224,449,101,1,223,223,1108,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,509,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,677,226,224,102,2,223,223,1006,224,554,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,569,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,629,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,644,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,659,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

testInput :: Memory
testInput = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

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

systemID :: Value
systemID = 5

compute :: Memory -> Location -> Output
compute mem pointer = let vals = drop pointer mem in
                      let Code c b a op = getOpcode (head vals) in
                          case op of
                              Add       -> compute (write mem (vals!!3) (getValue mem (vals!!1) a + getValue mem (vals!!2) b)) (pointer + 4)

                              Mult      -> compute (write mem (vals!!3) (getValue mem (vals!!1) a * getValue mem (vals!!2) b)) (pointer + 4)

                              Input     -> compute (write mem (vals!!1) systemID) (pointer + 2)

                              Output    -> let (mem', out) = compute mem (pointer + 2) in
                                                  (mem', (getValue mem (vals!!1) a):out)

                              JumpTrue  -> if (getValue mem (vals!!1) a) == 0 then 
                                            compute mem (pointer + 3)
                                            else compute mem (getValue mem (vals!!2) b)

                              JumpFalse -> if (getValue mem (vals!!1) a) == 0 then 
                                             compute mem (getValue mem (vals!!2) b)
                                             else compute mem (pointer + 3)

                              LessThan  -> if (getValue mem (vals!!1) a) < (getValue mem (vals!!2) b) then
                                             compute (write mem (vals!!3) 1) (pointer + 4)
                                             else compute (write mem (vals!!3) 0) (pointer + 4)

                              Equal     -> if (getValue mem (vals!!1) a) == (getValue mem (vals!!2) b) then
                                             compute (write mem (vals!!3) 1) (pointer + 4)
                                             else compute (write mem (vals!!3) 0) (pointer + 4)

                              Halt      -> (mem, [])

run :: Memory -> [Value]
run mem = snd (compute mem 0)
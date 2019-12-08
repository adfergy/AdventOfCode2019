type Value = Int
type Memory = [Value]
type Location = Int
type Mode = Int
type Output = (Memory, Location, Maybe (Value))
type Input = [Int]
type PhaseSetting = Int

data Opcode = Code Mode Mode Mode Operation
    deriving Show

data Operation = Add | Mult | Input | Output | JumpTrue | JumpFalse | LessThan | Equal | Halt 
    deriving Show

testInputA :: Memory
testInputA = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

testInputB :: Memory
testInputB = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

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

                                          Output    -> (mem, (pointer + 2), Just (getValue mem (vals!!1) a))

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

                                          Halt      -> (mem, pointer, Nothing)

permutations :: [PhaseSetting] -> [[PhaseSetting]]
permutations [] = [[]]
permutations ps = concat (map (\p -> map (\ps -> p:ps) (permutations (filter (\x -> not (x==p)) ps))) ps)

amplifier :: [PhaseSetting] -> [Value] -> Location -> Memory -> Output
amplifier ps v pc mem = (compute mem pc (ps++v))

calculateThrust :: [PhaseSetting] -> Memory -> Value
calculateThrust [ps1, ps2, ps3, ps4, ps5] mem = 
  let (mem1, pc1, out1) = amplifier [ps1] [0] 0 mem in
  let (mem2, pc2, out2) = amplifier [ps2] (deMaybe out1) 0 mem in 
  let (mem3, pc3, out3) = amplifier [ps3] (deMaybe out2) 0 mem in 
  let (mem4, pc4, out4) = amplifier [ps4] (deMaybe out3) 0 mem in 
  let (mem5, pc5, out5) = amplifier [ps5] (deMaybe out4) 0 mem in
  calculateThrustLoop [ps1, ps2, ps3, ps4, ps5] [(mem1, pc1, out1), (mem2, pc2, out2), (mem3, pc3, out3), (mem4, pc4, out4), (mem5, pc5, out5)] (deMaybe out5)

calculateThrustLoop :: [PhaseSetting] -> [Output] -> [Value] -> Value
calculateThrustLoop _ [_, _, _, _, (_, _, Nothing)] outs = head outs
calculateThrustLoop ps [(mem1, pc1, _), (mem2, pc2, _), (mem3, pc3, _), (mem4, pc4, _), (mem5, pc5, Just(o))] outs =  
  let [ps1, ps2, ps3, ps4, ps5] = ps in
  let (mem1', pc1', out1') = amplifier [] [o] pc1 mem1 in
  let (mem2', pc2', out2') = amplifier [] (deMaybe out1') pc2 mem2 in 
  let (mem3', pc3', out3') = amplifier [] (deMaybe out2') pc3 mem3 in 
  let (mem4', pc4', out4') = amplifier [] (deMaybe out3') pc4 mem4 in 
  let (mem5', pc5', out5') = amplifier [] (deMaybe out4') pc5 mem5 in
  calculateThrustLoop ps [(mem1', pc1', out1'), (mem2', pc2', out2'), (mem3', pc3', out3'), (mem4', pc4', out4'), (mem5', pc5', out5')] ((deMaybe out5')++outs)

deMaybe :: Maybe (Value) -> [Value]
deMaybe (Nothing) = []
deMaybe (Just(a)) = [a]

answer :: Value
answer = maximum (map (\ps -> calculateThrust ps input) (permutations [5..9]))

-- answer = 1047153
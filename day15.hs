import qualified Data.Map as Map

type Value = Integer
type Memory = [Value]
type Location = Integer
type Mode = Integer
type Output = (Memory, Value, Integer, Integer)
type Base = Integer
type Input = [Integer]

data Opcode = Code Mode Mode Mode Operation
    deriving Show

data Operation = Add | Mult | Input | Output | JumpTrue | JumpFalse | LessThan | Equal | RelBase | Halt 
    deriving Show

robot :: Memory
robot = [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,101,0,1034,1039,1001,1036,0,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1001,1034,0,1039,1001,1036,0,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1106,0,124,1001,1034,-1,1039,1008,1036,0,1041,102,1,1035,1040,101,0,1038,1043,102,1,1037,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,101,0,1035,1040,1001,1038,0,1043,101,0,1037,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,1,1032,1006,1032,165,1008,1040,3,1032,1006,1032,165,1101,0,2,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,45,1044,1105,1,224,1101,0,0,1044,1106,0,224,1006,1044,247,1002,1039,1,1034,1002,1040,1,1035,1001,1041,0,1036,1002,1043,1,1038,102,1,1042,1037,4,1044,1106,0,0,7,39,95,7,98,8,11,47,17,33,19,4,29,41,87,34,59,22,75,5,1,46,41,29,32,11,55,25,53,41,77,27,52,33,41,65,72,24,43,83,72,3,14,92,2,43,82,30,87,19,94,47,91,10,8,67,24,4,68,85,63,4,93,29,55,34,23,65,40,3,36,90,57,97,37,2,65,8,1,16,83,93,67,44,71,97,27,70,76,20,40,90,36,73,27,89,57,13,66,37,95,76,26,84,33,48,34,86,85,30,81,6,61,33,83,84,22,21,67,27,11,49,28,69,41,60,98,6,69,41,54,82,18,37,65,10,42,47,41,2,72,16,66,39,93,37,2,41,52,49,20,78,30,7,38,15,40,81,21,14,82,44,48,7,96,33,36,70,52,18,71,1,81,66,47,1,38,78,80,38,63,53,80,16,58,55,93,31,89,36,36,78,65,71,34,83,4,55,60,29,10,30,84,15,59,31,96,16,21,58,26,38,35,58,50,16,46,25,26,82,59,12,11,98,4,17,42,66,83,72,23,14,92,22,9,5,87,5,79,85,19,87,71,28,61,32,56,92,56,19,78,94,39,24,73,58,28,37,81,11,99,25,46,73,44,5,22,41,76,55,84,31,16,36,65,84,40,29,81,66,16,94,23,54,23,29,51,20,25,23,69,44,23,18,99,80,55,39,10,71,7,33,63,94,93,62,26,35,25,50,61,39,84,38,54,43,56,23,67,17,70,34,23,90,93,24,46,60,31,46,33,53,81,10,62,23,89,86,43,39,73,82,38,9,61,42,66,68,30,28,95,4,25,54,22,21,80,32,61,13,6,66,47,59,4,31,59,17,87,72,30,72,51,30,30,62,43,53,88,42,48,13,21,80,8,30,61,14,77,22,27,60,87,30,65,14,33,76,67,9,95,26,84,40,21,52,11,86,23,30,86,57,28,6,69,4,11,63,21,2,65,51,39,58,82,16,51,96,23,3,44,21,62,31,38,47,73,30,29,94,24,14,88,1,51,72,42,57,48,63,33,95,78,15,17,68,64,61,10,31,58,68,36,15,52,19,13,26,38,72,41,66,15,56,88,18,98,87,15,43,89,96,3,94,55,25,26,27,6,48,3,29,90,88,6,18,29,88,90,43,3,81,61,16,31,93,42,26,46,31,56,66,17,76,37,15,50,33,81,16,10,83,87,37,39,92,80,62,6,59,77,9,32,91,61,97,24,44,62,61,11,36,94,59,54,34,23,67,18,86,31,39,77,73,44,67,27,57,5,54,65,29,21,81,2,65,39,24,82,6,55,33,97,72,35,16,85,19,28,57,94,21,15,86,5,52,53,39,69,20,32,52,5,86,95,44,47,77,9,57,14,62,49,54,7,70,29,16,42,87,99,30,36,67,68,14,42,73,4,87,97,39,61,18,11,39,77,83,17,83,27,1,72,30,21,95,38,35,96,15,78,27,66,40,4,95,90,94,4,20,63,71,19,54,11,28,96,46,13,42,94,84,9,22,79,37,14,50,13,58,64,90,30,69,18,20,90,4,21,31,95,88,22,81,36,20,11,82,59,95,38,43,72,3,78,38,33,62,48,36,22,16,3,87,53,91,37,12,19,49,18,25,14,67,78,79,9,70,88,34,98,38,8,90,98,56,13,26,34,82,77,40,97,82,63,32,57,26,58,53,29,56,3,62,17,78,67,69,33,49,62,47,36,60,9,81,12,96,6,78,86,98,34,70,41,87,86,47,15,46,36,49,20,76,31,48,1,68,19,96,0,0,21,21,1,10,1,0,0,0,0,0,0]

lookupV :: Memory -> Location -> Value
lookupV mem n = if n >= (toInteger.length) mem then 0 else (mem!!(fromIntegral n))

write :: Memory -> Location -> Value -> Memory
write mem n v = (take (fromIntegral n) (mem ++ repeat 0)) ++ [v] ++ (drop (fromIntegral n+1) mem)

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
                  9  -> RelBase 
                  99 -> Halt
                  n  -> error ("*** unimplemented opcode " ++ (show n) ++ " ***")

getValue :: Memory -> Value -> Mode -> Base -> Value
getValue mem v m rb = case m of
                        0 -> lookupV mem v
                        1 -> v
                        2 -> lookupV mem (v + rb)

getAddress :: Location -> Mode -> Base -> Value
getAddress l m rb = case m of
                          2 -> (l+rb)
                          _ -> l

digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

step :: Memory -> Location -> Base -> Input -> Output
step mem pc rb inp = let [p1, p2, p3, p4] = take 4 (drop (fromIntegral pc) (mem ++ repeat 0)) in
                        let Code c b a op = getOpcode p1 in
                        let [v1, v2, v3] = map (\(p,m) -> getValue mem p m rb) [(p2, a), (p3,b), (p4,c)] in
                        let [a1, a2, a3] = map (\(p,m) -> getAddress p m rb) [(p2, a), (p3,b), (p4,c)] in
                            case op of
                                Add       -> step (write mem a3 (v1 + v2)) (pc + 4) rb inp

                                Mult      -> step (write mem a3 (v1 * v2)) (pc + 4) rb inp

                                Input     -> step (write mem a1 (head inp)) (pc + 2) rb (tail inp)

                                Output    -> (mem, v1, (pc+2), rb)

                                JumpTrue  -> step mem (if (v1==0) then (pc + 3) else v2) rb inp

                                JumpFalse -> step mem (if (v1==0) then v2 else (pc + 3)) rb inp

                                LessThan  -> step (write mem a3 (if (v1 < v2) then 1 else 0)) (pc + 4) rb inp

                                Equal     -> step (write mem a3 (if (v1 == v2) then 1 else 0)) (pc + 4) rb inp

                                RelBase   -> step mem (pc + 2) (rb + v1) inp

                                Halt      -> error "HALT" -- (mem, (-1), pc, rb)

type CoOrd = (Integer, Integer)

type SeenBy = Map.Map CoOrd Int

calcLoc :: CoOrd -> [Integer] -> CoOrd
calcLoc (x,y) [] = (x,y)
calcLoc (x,y) (d:ds)
  | d == 1 = calcLoc (x,y+1) ds
  | d == 2 = calcLoc (x,y-1) ds
  | d == 3 = calcLoc (x+1,y) ds
  | d == 4 = calcLoc (x-1,y) ds

haventSeen :: SeenBy -> CoOrd -> Int -> Bool
haventSeen seen loc by = case Map.lookup loc seen of
                        Nothing -> True
                        Just (x) -> x > by

insertSeen :: SeenBy -> [(CoOrd, Int)] -> SeenBy
insertSeen seen [] = seen
insertSeen seen (((x,y),a):xs) = case Map.lookup (x,y) (insertSeen seen xs) of
                                  Nothing -> Map.insert (x,y) a seen
                                  Just (b) -> seen 

search :: [(Memory, Location, Value, [Integer])] -> SeenBy -> (Int, CoOrd, (Memory, Location, Value))
search [] seen =  let (umem, up, upc, urb) = step robot 0 0 [1] in 
                  let (dmem, down, dpc, drb) = step robot 0 0 [2] in 
                  let (lmem, left, lpc, lrb) = step robot 0 0 [3] in 
                  let (rmem, right, rpc, rrb) = step robot 0 0 [4] in 
                  if up == 2 then (1, (0,1), (umem,upc,urb)) else 
                  if down == 2 then (1, (0,-1), (dmem,dpc,drb)) else 
                  if left == 2 then (1, (1,0), (lmem,lpc,lrb)) else 
                  if right == 2 then (1, (-1,0), (rmem,rpc,rrb)) else 
                  let up' = if (up==1) then [(umem,upc,urb,[1])] else [] in 
                  let down' = if (down==1) then [(dmem,dpc,drb,[2])] else [] in 
                  let left' = if (left==1) then [(lmem,lpc,lrb,[3])] else [] in 
                  let right' = if (right==1) then [(rmem,rpc,rrb,[4])] else [] in 
                  search (up' ++ down' ++ left' ++ right') (insertSeen seen [((0,1), 1),((0,-1), 1),((-1,0), 1),((1,0), 1)])

search ((mem, pc, rb, ds):cs) seen =  let (umem, up, upc, urb) = step mem pc rb [1] in 
                                      let (dmem, down, dpc, drb) = step mem pc rb [2] in 
                                      let (lmem, left, lpc, lrb) = step mem pc rb [3] in 
                                      let (rmem, right, rpc, rrb) = step mem pc rb [4] in  
                                      let (x,y) = calcLoc (0,0) ds in
                                      if up == 2 then (1 + length ds, (x,y+1), (umem,upc,urb)) else 
                                      if down == 2 then (1 + length ds, (x,y-1), (dmem,dpc,drb)) else 
                                      if left == 2 then (1 + length ds, (x+1,y), (lmem,lpc,lrb)) else 
                                      if right == 2 then (1 + length ds, (x-1,y), (rmem,rpc,rrb)) else 
                                      let (up',wallUp) = if (up==1) && (haventSeen seen (x,y+1) (length ds)) then ([(umem,upc,urb,ds ++ [1])], []) else ([],[((x,y+1), length ds)]) in 
                                      let (down',wallDown) = if (down==1) && (haventSeen seen (x,y-1) (length ds)) then ([(dmem,dpc,drb,ds ++ [2])], []) else ([],[((x,y-1), length ds)]) in 
                                      let (left',wallLeft) = if (left==1) && (haventSeen seen (x+1,y) (length ds)) then ([(lmem,lpc,lrb,ds ++ [3])], []) else ([],[((x+1,y), length ds)]) in 
                                      let (right',wallRight) = if (right==1) && (haventSeen seen (x-1,y) (length ds)) then ([(rmem,rpc,rrb,ds ++ [4])], []) else ([],[((x-1,y), length ds)]) in 
                                      search (cs ++ up' ++ down' ++ left' ++ right') (insertSeen seen ([((x,y),length ds)]++wallUp++wallDown++wallLeft++wallRight))

answerA :: Int
answerA = (\(x,_,_) -> x) (search [] (Map.fromList [((0,0),0)]))

--main = do putStr (show answerA)
--answerA = 246

--Part B                

oxLoc :: CoOrd
oxLoc = (20,18)

distanceTo :: SeenBy -> CoOrd -> Int -> Int
distanceTo seen c a = case Map.lookup c seen of
                        Nothing -> a
                        Just (b) -> b

fill :: [(Memory, Location, Value, [Integer])] -> SeenBy -> Int
fill [] seen =  let (mem, pc, rb) = (\(_,_,x) -> x) (search [] (Map.fromList [((0,0),0)])) in
                let (umem, up, upc, urb) = step mem pc rb [1] in 
                let (dmem, down, dpc, drb) = step mem pc rb [2] in 
                let (lmem, left, lpc, lrb) = step mem pc rb [3] in 
                let (rmem, right, rpc, rrb) = step mem pc rb [4] in 
                let (x,y) = oxLoc in
                let up' = if (up==1) then [(umem,upc,urb,[1])] else [] in 
                let down' = if (down==1) then [(dmem,dpc,drb,[2])] else [] in 
                let left' = if (left==1) then [(lmem,lpc,lrb,[3])] else [] in 
                let right' = if (right==1) then [(rmem,rpc,rrb,[4])] else [] in 
                fill (up' ++ down' ++ left' ++ right') (insertSeen seen [((x,y+1), 1),((x,y-1), 1),((x-1,y), 1),((x+1,y), 1)])

fill ((mem, pc, rb, ds):cs) seen = 
                      let (umem, up, upc, urb) = step mem pc rb [1] in 
                      let (dmem, down, dpc, drb) = step mem pc rb [2] in 
                      let (lmem, left, lpc, lrb) = step mem pc rb [3] in 
                      let (rmem, right, rpc, rrb) = step mem pc rb [4] in 
                      let (x,y) = calcLoc (0,0) ds in
                      let (up',wallUp) = if (up==1) && (haventSeen seen (x,y+1) (length ds)) then ([(umem,upc,urb,ds ++ [1])], []) else ([],[((x,y+1), length ds)]) in 
                      let (down',wallDown) = if (down==1) && (haventSeen seen (x,y-1) (length ds)) then ([(dmem,dpc,drb,ds ++ [2])], []) else ([],[((x,y-1), length ds)]) in 
                      let (left',wallLeft) = if (left==1) && (haventSeen seen (x+1,y) (length ds)) then ([(lmem,lpc,lrb,ds ++ [3])], []) else ([],[((x+1,y), length ds)]) in 
                      let (right',wallRight) = if (right==1) && (haventSeen seen (x-1,y) (length ds)) then ([(rmem,rpc,rrb,ds ++ [4])], []) else ([],[((x-1,y), length ds)]) in 
                      if up' == [] && down' == [] && left' == [] && right' == [] && cs == [] then (distanceTo seen (x,y) (length ds)) else
                      fill (cs ++ up' ++ down' ++ left' ++ right') (insertSeen seen ([((x,y),length ds)]++wallUp++wallDown++wallLeft++wallRight))

answerB :: Int
answerB = fill [] (Map.fromList [(oxLoc,0)])

--main = do putStr (show answerB)
--answerB = 376
import Data.List
type Value = Integer
type Memory = [Value]
type Location = Integer
type Mode = Integer
type Output = (Memory, Value, Location, Location)
type Base = Integer
type Input = [Integer]

data Opcode = Code Mode Mode Mode Operation
    deriving Show

data Operation = Add | Mult | Input | Output | JumpTrue | JumpFalse | LessThan | Equal | RelBase | Halt 
    deriving Show

mainInput :: Memory
mainInput = [3,8,1005,8,291,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,1,1003,20,10,2,1103,19,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,59,1,1004,3,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,84,1006,0,3,1,1102,12,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,114,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,135,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,158,2,9,9,10,2,2,10,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,188,1006,0,56,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,212,1006,0,76,2,1005,8,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,241,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,264,1006,0,95,1,1001,12,10,101,1,9,9,1007,9,933,10,1005,10,15,99,109,613,104,0,104,1,21102,838484206484,1,1,21102,1,308,0,1106,0,412,21102,1,937267929116,1,21101,0,319,0,1105,1,412,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,206312598619,1,1,21102,366,1,0,1105,1,412,21101,179410332867,0,1,21102,377,1,0,1105,1,412,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,709580595968,1,21102,1,400,0,1106,0,412,21102,868389384552,1,1,21101,411,0,0,1106,0,412,99,109,2,21202,-1,1,1,21102,1,40,2,21102,1,443,3,21101,0,433,0,1106,0,476,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,438,439,454,4,0,1001,438,1,438,108,4,438,10,1006,10,470,1102,0,1,438,109,-2,2106,0,0,0,109,4,1202,-1,1,475,1207,-3,0,10,1006,10,493,21102,0,1,-3,21202,-3,1,1,21201,-2,0,2,21101,0,1,3,21102,1,512,0,1106,0,517,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,540,2207,-4,-2,10,1006,10,540,22101,0,-4,-4,1106,0,608,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21101,0,559,0,1106,0,517,21201,1,0,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,578,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,600,21201,-1,0,1,21102,600,1,0,106,0,475,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]


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

compute :: Memory -> Location -> Base -> Input -> Output
compute mem pc rb inp = let [p1, p2, p3, p4] = take 4 (drop (fromIntegral pc) (mem ++ repeat 0)) in
                        let Code c b a op = getOpcode p1 in
                        let [v1, v2, v3] = map (\(p,m) -> getValue mem p m rb) [(p2, a), (p3,b), (p4,c)] in
                        let [a1, a2, a3] = map (\(p,m) -> getAddress p m rb) [(p2, a), (p3,b), (p4,c)] in
                            case op of
                                Add       -> compute (write mem a3 (v1 + v2)) (pc + 4) rb inp

                                Mult      -> compute (write mem a3 (v1 * v2)) (pc + 4) rb inp

                                Input     -> compute (write mem a1 (head inp)) (pc + 2) rb (tail inp)

                                Output    -> (mem, v1, (pc+2), rb)

                                JumpTrue  -> compute mem (if (v1==0) then (pc + 3) else v2) rb inp

                                JumpFalse -> compute mem (if (v1==0) then v2 else (pc + 3)) rb inp

                                LessThan  -> compute (write mem a3 (if (v1 < v2) then 1 else 0)) (pc + 4) rb inp

                                Equal     -> compute (write mem a3 (if (v1 == v2) then 1 else 0)) (pc + 4) rb inp

                                RelBase   -> compute mem (pc + 2) (rb + v1) inp

                                Halt      -> (mem, (-1), (-1), rb)


type CoOrd = (Integer, Integer)
type Panel = []

--                mem      pc          rb         x,y      direcn     whites     edited      num      whites
paintingRobot :: Memory -> Location -> Location -> CoOrd -> Integer -> [CoOrd] -> [CoOrd] -> (Int, [CoOrd])

paintingRobot mem pc rb c dn ws cs = let (mem', o1, pc', rb') = compute mem pc rb (repeat (if elem c ws then 1 else 0)) in
                                     if (pc' == (-1)) then (length cs, ws) else
                                     let ws' = (if o1 == 1 then (ws `union` [c]) else (ws \\ [c])) in                                     
                                     let (mem'', d1, pc'', rb'') = compute mem' pc' rb' (repeat (if elem c ws' then 1 else 0)) in
                                            let dn' = (dn + (if d1==0 then (-1) else 1)) `mod` 4 in
                                            let c' = getNewLoc c dn' in
                                            let cs' = cs `union` [c] in
                                                paintingRobot mem'' pc'' rb'' c' dn' ws' cs'

getNewLoc :: CoOrd -> Integer -> CoOrd
getNewLoc (x,y) dn
    | dn == 0 = (x  , y-1)
    | dn == 1 = (x+1, y  )
    | dn == 2 = (x  , y+1)
    | dn == 3 = (x-1, y  )

answerA :: Int
answerA = fst (paintingRobot mainInput 0 0 (0,0) 0 [] [])


getLocs :: [CoOrd]
getLocs = snd (paintingRobot mainInput 0 0 (0,0) 0 [(0,0)] [])

locs :: [(Int, Int)]
locs = map (\(x,y) -> (fromInteger(x- minimum (map fst getLocs)), fromInteger( y - minimum (map snd getLocs)))) getLocs

width :: Int
width = 1 + maximum (map fst locs)

height :: Int 
height =  1 + maximum (map snd locs) 

image :: [String]
image = take height (repeat (take width (repeat ' ')))

getImage :: [String] -> [(Int, Int)] -> [String]
getImage im [] = im
getImage im ((x,y):cs) = let row = take (x) (im!!y) ++ "X" ++ drop (x+1) (im!!y) in
                          getImage (take (y) im ++ [row] ++ drop (y+1) im) cs 

drawImage :: IO ()
drawImage = putStr(foldr (\x xs -> x ++ '\n':xs) ""  (getImage image locs))

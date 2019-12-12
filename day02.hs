 -- Part A

type Memory = [Int]

inputData :: Memory
inputData = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,1,13,23,27,1,6,27,31,2,31,13,35,1,9,35,39,2,39,13,43,1,43,10,47,1,47,13,51,2,13,51,55,1,55,9,59,1,59,5,63,1,6,63,67,1,13,67,71,2,71,10,75,1,6,75,79,1,79,10,83,1,5,83,87,2,10,87,91,1,6,91,95,1,9,95,99,1,99,9,103,2,103,10,107,1,5,107,111,1,9,111,115,2,13,115,119,1,119,10,123,1,123,10,127,2,127,10,131,1,5,131,135,1,10,135,139,1,139,2,143,1,6,143,0,99,2,14,0,0]


add :: Memory -> Int -> Int -> Int -> Memory
add xs a b i = let s = xs!!a + xs!!b in 
                                take i xs ++ [s] ++ drop (i+1) xs

mult :: Memory -> Int -> Int -> Int -> Memory
mult xs a b i = let s = xs!!a * xs!!b in 
                                take i xs ++ [s] ++ drop (i+1) xs

compute :: Memory -> Int -> Memory
compute memory i =  let input = drop i memory in
                    case head(input) of 99 -> memory
                                        1  -> compute (add memory (input!!1) (input!!2) (input!!3)) (i+4)
                                        2  -> compute (mult memory (input!!1) (input!!2) (input!!3)) (i+4)
                                        _  -> error "Opcode Not Implemented"

-- compute inputData 0 = 1450679,0,0,2,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,0,1,5,19,1,1,13,23,6,1,6,27,8,2,31,13,40,1,9,35,43,2,39,13,215,1,43,10,219,1,47,13,224,2,13,51,1120,1,55,9,1123,1,59,5,1124,1,6,63,1126,1,13,67,1131,2,71,10,4524,1,6,75,4526,1,79,10,4530,1,5,83,4531,2,10,87,18124,1,6,91,18126,1,9,95,18129,1,99,9,18132,2,103,10,72528,1,5,107,72529,1,9,111,72532,2,13,115,362660,1,119,10,362664,1,123,10,362668,2,127,10,1450672,1,5,131,1450673,1,10,135,1450677,1,139,2,1450677,1,6,143,0,99,2,14,0,0

replace12 :: Int -> Int -> Memory -> Memory
replace12 a b xs = [head(xs),a,b] ++ drop 3 xs

answerA :: Int
answerA = head (compute (replace12 12 2 inputData) 0)

-- answerA = 5290681

-- Part B

type Noun = Int
type Verb = Int

possNounVerb :: [(Noun, Verb)]
possNounVerb = let xs = [0..99] in [ (x,y) | x <- xs, y <- xs]

findNounVerb :: [(Noun, Verb)] -> (Noun, Verb)
findNounVerb [] = error "No Matching Pair"
findNounVerb ((a,b):xs) = case head (compute (replace12 a b inputData) 0) of 19690720 -> (a,b)
                                                                             _          -> findNounVerb(xs)

answerB :: Int
answerB = let (noun, verb) = findNounVerb possNounVerb in (100 * noun + verb)

--answerB = 5741

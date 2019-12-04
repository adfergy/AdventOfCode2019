--PartA

inputMin :: Int 
inputMin = 271973

inputMax :: Int 
inputMax = 785961

rangeOfInts :: [Int]
rangeOfInts = [inputMin .. inputMax]

validPassword :: Int -> Bool
validPassword n = (checkPairwise n (<=) andAll) && (checkPairwise n (==) orAll)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

andAll :: [Bool] -> Bool
andAll = foldr (&&) True

orAll :: [Bool] -> Bool
orAll = foldr (||) False 

checkPairwise :: Int -> (Int -> Int -> Bool) -> ([Bool] -> Bool) -> Bool
checkPairwise n f b = let ds = digs n in 
                        b (zipWith f ds (tail ds))

answerA :: Int
answerA = length (filter validPassword rangeOfInts)

--Part B

hasDouble :: Int -> Bool
hasDouble n = elem 2 (countRun (digs n) (-1) 1)

countRun :: [Int] -> Int -> Int -> [Int]
countRun [] k n = [n]
countRun (x:xs) k n = if k==x then countRun xs x (n+1)
                      else n:(countRun xs x 1)  

validPassword2 :: Int -> Bool 
validPassword2 n = (checkPairwise n (<=) andAll) && (hasDouble n)

answerB :: Int
answerB = length (filter validPassword2 rangeOfInts)
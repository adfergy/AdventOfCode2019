import Data.List
type Map = [String]
type Locations = [CoOrd]
type CoOrd = (Integer, Integer)

inputMap :: Map
inputMap = ["....#...####.#.#...........#........","#####..#.#.#......#####...#.#...#...","##.##..#.#.#.....#.....##.#.#..#....","...#..#...#.##........#..#.......#.#","#...##...###...###..#...#.....#.....","##.......#.....#.........#.#....#.#.","..#...#.##.##.....#....##..#......#.","..###..##..#..#...#......##...#....#","##..##.....#...#.#...#......#.#.#..#","...###....#..#.#......#...#.......#.","#....#...##.......#..#.......#..#...","#...........#.....#.....#.#...#.##.#","###..#....####..#.###...#....#..#...","##....#.#..#.#......##.......#....#.","..#.#....#.#.#..#...#.##.##..#......","...#.....#......#.#.#.##.....#..###.","..#.#.###.......#..#.#....##.....#..",".#.#.#...#..#.#..##.#..........#...#",".....#.#.#...#..#..#...###.#...#.#..","#..#..#.....#.##..##...##.#.....#...","....##....#.##...#..........#.##....","...#....###.#...##........##.##..##.","#..#....#......#......###...........","##...#..#.##.##..##....#..#..##..#.#",".#....#..##.....#.#............##...",".###.........#....#.##.#..#.#..#.#..","#...#..#...#.#.#.....#....#......###","#...........##.#....#.##......#.#..#","....#...#..#...#.####...#.#..#.##...","......####.....#..#....#....#....#.#",".##.#..###..####...#.......#.#....#.","#.###....#....#..........#.....###.#","...#......#....##...##..#..#...###..","..#...###.###.........#.#..#.#..#...",".#.#.............#.#....#...........","..#...#.###...##....##.#.#.#....#.#."]

testInputA :: Map
testInputA = [".#..##.###...#######","##.############..##.",".#.######.########.#",".###.#######.####.#.","#####.##.#.##.###.##","..#####..#.#########","####################","#.####....###.#.#.##","##.#################","#####.##.###..####..","..######..##.#######","####.##.####...##..#",".#####..#.######.###","##...#.##########...","#.##########.#######",".####.#.###.###.#.##","....##.##.###..#####",".#.#.###########.###","#.#.#.#####.####.###","###.##.####.##.#..##"]

testInputB :: Map
testInputB = ["......#.#.","#..#.#....","..#######.",".#.#.###..",".#..#.....","..#....#.#","#..#....#.",".##.#..###","##...#..#.",".#....####"]

testInputC :: Map
testInputC = ["#....",".#...","..#..","...#.","....#"]

testInputD :: Map
testInputD = [".#..#..###","####.###.#","....###.#.","..###.##.#","##.##.#.#.","....###..#","..#.#..#.#","#..#.#.###",".##...##.#",".....#.#.."]

testInputE :: Map
testInputE = ["#.#...#.#.",".###....#.",".#....#...","##.#.#.#.#","....#.#.#.",".##..###.#","..#...##..","..##....##","......#...",".####.###."]

mapToCoOrds :: Map -> Locations
mapToCoOrds m = concat ( zipWith rowToCords [0..] m )

rowToCords :: Integer -> String -> Locations
rowToCords y s = concat ( zipWith (isAst y) [0..] s )

isAst :: Integer -> Integer -> Char -> Locations
isAst y x '#' = [(x,y)]
isAst _ _ '.' = []

primes :: [Integer]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

primeFactorise :: Integer -> [Integer]
primeFactorise n = let ps = concat (map (\p -> (if n `mod` p == 0 then [p] else [])) primes) in
                   ps ++ (if ps==[] then [] else (primeFactorise (foldl (div) n ps)))


reduceFraction :: Integer -> Integer -> (Integer, Integer)
reduceFraction a b = let as = primeFactorise a in
                     let bs = primeFactorise b in
                     (product (as \\ bs), product (bs \\ as))

inBetween :: CoOrd -> CoOrd -> [CoOrd]
inBetween (x1, y1) (x2, y2) 
            | x1 == x2 = map (\c -> (x1, c)) [minimum[y1,y2]..maximum[y1,y2]]
            | y1 == y2 = map (\c -> (c, y1)) [minimum[x1,x2]..maximum[x1,x2]]
            | x1 < x2  = let (xc, yc) = reduceFraction (x1 - x2) (y1 - y2) in
                         let yc' = yc * (if y1 >= y2 then (-1) else (1)) in
                            getInb (x1, y1) (x2, y2) xc yc'
            | x1 > x2  = let (xc, yc) = reduceFraction (x1 - x2) (y1 - y2) in
                         let yc' = yc * (if y1 < y2 then (-1) else (1)) in
                            getInb (x2, y2) (x1, y1) xc yc'

getInb :: CoOrd -> CoOrd -> Integer -> Integer -> [CoOrd]
getInb (x1, y1) (x2, y2) xc yc
        | x1 == x2 && y1 == y2 = [(x1, y1)]
        | otherwise = (x1,y1):(getInb (x1 + xc, y1 + yc) (x2, y2) xc yc)

canSee :: CoOrd -> CoOrd -> [CoOrd] -> Bool
canSee c1 c2 ls  
            | c1 == c2 = False
            | otherwise = let is = inBetween c1 c2 in
                            (is \\ (ls \\ [c1,c2])) == is  

numCanSee :: CoOrd -> [CoOrd] -> Int
numCanSee c1 ls = length ( filter id (map (\c2 -> canSee c1 c2 ls) ls)) 

answerA :: (Int, CoOrd)
answerA = let ls = mapToCoOrds inputMap in
           maximum (map (\c -> (numCanSee c ls, c)) ls)

-- answerA = (329, (25, 31))

testInputF :: Map
testInputF = [".#....#####...#..","##...##.#####..##","##...#...#.#####.","..#.....#...###..","..#.#.....#....##"]

visible :: CoOrd -> [CoOrd] -> [CoOrd]
visible c1 ls = filter (\c2 -> canSee c1 c2 ls) ls

revolution :: CoOrd -> [CoOrd] -> [CoOrd] -> [(Double, CoOrd)]
revolution (x1, y1) ls [] = []
revolution (x1, y1) ls vs = (sortBy sortAng (map (\c -> (angle (x1, y1) c, c)) vs)) ++ (revolution (x1, y1) (ls\\vs) (visible (x1, y1) (ls\\vs))) 

angle :: CoOrd -> CoOrd -> Double
angle (cx, cy) (x, y) = atan2 (fromInteger (x - cx))  (fromInteger ((y - cy)*(-1)))

sortAng :: (Double, CoOrd) -> (Double, CoOrd) -> Ordering
sortAng (t1, _) (t2, _)
    | t1 >= 0 && t2 < 0 = LT 
    | t1 < 0 && t2 >= 0 = GT 
    | otherwise = compare t1 t2

answerB :: Integer
answerB = let c = (25,31) in
          let ls = (mapToCoOrds inputMap) in 
          let vs = visible c ls in
          let (x,y) = snd ((revolution c ls vs)!!199) in
          x * 100 + y

-- answerB = 512
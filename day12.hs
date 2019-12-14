import Data.List
type Position = (Integer, Integer, Integer)
type Velocity = (Integer, Integer, Integer)
type Planet = (Position, Velocity)
type Energy = Integer

test :: [Planet]
test = [((-1,0,2),(0,0,0)),((2,-10,-7),(0,0,0)),((4,-8,8),(0,0,0)),((3,5,-1),(0,0,0))]

test2 :: [Planet]
test2 = [((-8,-10,0),(0,0,0)),((5,5,10),(0,0,0)),((2,-7,3),(0,0,0)),((9,-8,-3),(0,0,0))]

io :: Planet
io = ((7, 10, 17), (0,0,0))

europa :: Planet
europa = ((-2, 7, 0), (0,0,0))

ganymede :: Planet
ganymede = ((12, 5, 12), (0,0,0))

callisto :: Planet
callisto = ((5, -8, 6), (0,0,0))

planets :: [Planet]
planets = [io, europa, ganymede, callisto]

movePlanet :: Planet -> Position
movePlanet ((xp,yp,zp), (xv,yv,zv)) = (xp+xv, yp+yv, zp+zv)

calcVelocityCh :: Position -> Position -> Velocity
calcVelocityCh (x1, y1, z1) (x2, y2, z2) = let x1v = if x1 < x2 then 1 else (if x1 > x2 then -1 else 0) in
                                           let y1v = if y1 < y2 then 1 else (if y1 > y2 then -1 else 0) in
                                           let z1v = if z1 < z2 then 1 else (if z1 > z2 then -1 else 0) in
                                           (x1v, y1v, z1v)


calcVelocity :: Planet -> [Position] -> Velocity
calcVelocity ((x, y, z),(xv, yv, zv)) ps = let chs = map (\c -> calcVelocityCh (x,y,z) c) ps in
                                           foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (xv, yv, zv) chs



stepPlanet :: Planet -> [Planet] -> Planet
stepPlanet ((x,y,z),(xv,yv,zv)) ps = let (xv',yv',zv') = calcVelocity ((x,y,z),(xv,yv,zv)) (map fst ps) in
                                     let (x',y',z') = movePlanet ((x,y,z),(xv',yv',zv')) in
                                     ((x',y',z'),(xv',yv',zv'))

stepSys :: [Planet] -> [Planet]
stepSys ps = map (\p -> stepPlanet p ps) ps

absSum :: (Integer,Integer,Integer) -> Integer
absSum (x,y,z) = abs x + abs y + abs z

energyPlanet :: Planet -> Energy
energyPlanet (c,v) = absSum c * absSum v

energySys :: [Planet] -> Energy
energySys ps = sum (map energyPlanet ps)

--totalEn :: [Planet] -> Integer -> Energy
totalEn ps 0 = energySys ps
totalEn ps n = totalEn (stepSys ps) (n-1) 

answerA :: Energy
answerA = totalEn planets 1000

--answerA = 9958

-- Part B
type Position1D = Integer
type Velocity1D = Integer
type Planet1D = (Position1D, Velocity1D)

movePlanet' :: Planet1D -> Position1D
movePlanet' (x, v) = x + v

calcVelocityCh' :: (Position1D, Position1D) -> Velocity1D
calcVelocityCh' (x1, x2) = if (x1<x2) then 1 else (if x1 > x2 then -1 else 0)

calcVelocity' :: Planet1D -> [Position1D] -> Velocity1D
calcVelocity' (x,xv) ps = let chs = map (\c -> calcVelocityCh' (x,c)) ps in
                          sum chs + xv

stepPlanet' :: Planet1D -> [Planet1D] -> Planet1D
stepPlanet' (x,xv) ps = let xv' = calcVelocity' (x,xv) (map fst ps) in
                       let x' = movePlanet' (x,xv') in
                       (x',xv')

stepSys' :: [Planet1D] -> [Planet1D]
stepSys' ps = map (\p -> stepPlanet' p ps) ps

numSteps' :: Int -> [Planet1D] -> [Planet1D] -> (Int, Int)
numSteps' n is ss
                | is == ss && n>0 = (0,n)
                | otherwise = numSteps' (n+1) is (stepSys' ss)


--numSteps :: [Planet1D] -> [[Planet1D]] -> (Int, Int)
--numSteps ps ss =
--        case elemIndex ps ss of
--              Just x -> (x, length ss - x)
--              Nothing -> numSteps (stepSys' ps) (ss ++ [ps])

answerB :: [Planet] -> Int
answerB ps = let (sx, lx) = numSteps' 0 (map (\(a,b) -> (frst a, frst b)) ps) (map (\(a,b) -> (frst a, frst b)) ps) in
             let (sy, ly) = numSteps' 0 (map (\(a,b) -> (secn a, secn b)) ps) (map (\(a,b) -> (secn a, secn b)) ps) in
             let (sz, lz) = numSteps' 0 (map (\(a,b) -> (thrd a, thrd b)) ps) (map (\(a,b) -> (thrd a, thrd b)) ps) in
             let cm = lcm (lcm lx ly) lz in
             cm
             --sx + lx * frst (head(filter (\(a,b,c)-> (sx+a*lx)==(sy+b*ly) && (sy+b*ly)==(sz+c*lz)) [(x',y',z')|x'<-reverse [1..(cm `div` lx)],y'<-reverse [1..(cm `div` ly)],z'<-reverse [1..(cm `div` lz)]]))--

frst (x,_,_) = x
secn (_,x,_) = x
thrd (_,_,x) = x

main = do putStrLn (show (answerB planets))
--answerB = 318382803780324
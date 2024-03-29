import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map

type Chemical = String
type Ingredient = (Integer, Chemical)
type Recipie = Map.Map Chemical (Integer, [Ingredient])

testInputA :: String
testInputA = " 9 ORE => 2 A| 8 ORE => 3 B| 7 ORE => 5 C| 3 A, 4 B => 1 AB| 5 B, 7 C => 1 BC| 4 C, 1 A => 1 CA| 2 AB, 3 BC, 4 CA => 1 FUEL" 

puzzleInput :: String
puzzleInput = " 5 LKQCJ, 1 GDSDP, 2 HPXCL => 9 LVRSZ| 5 HPXCL, 5 PVJGF => 3 KZRTJ| 7 LVRSZ, 2 GFSZ => 5 FRWGJ| 9 ZPTXL, 5 HGXJH, 9 LQMT => 7 LVCXN| 2 LQMT, 2 PVJGF, 10 CKRVN => 9 VWJS| 2 VRMXL, 12 NBRCS, 2 WSXN => 7 GDSDP| 1 CKRP => 8 TBHVH| 1 SVMNB, 2 KZRTJ => 8 WKGQS| 6 LKQCJ, 8 HPXCL, 7 MPZH => 1 BQPG| 1 RCWL => 7 MPZH| 4 FGCMS, 2 LQMT, 1 LKQCJ => 1 KTBRM| 1 ZTCSK, 6 CXQB, 2 ZBZRT => 3 PVJGF| 7 DBNLM => 9 ZBZRT| 5 BGNQ, 2 WBPD, 5 KTBRM => 9 GFSZ| 6 XQBHG, 1 GPWVC => 8 CKFTS| 1 XWLQM, 29 XQBHG, 7 KPNWG => 5 BXVL| 6 TBHVH, 1 KTBRM => 7 HJGR| 1 LQMT, 14 KPNWG => 7 GPWVC| 18 LVCXN, 8 XVLT, 4 KPNWG, 13 LKQCJ, 12 MFJFW, 5 GZNJZ, 1 FLFT, 7 WBPD => 8 KZGD| 1 TBHVH => 1 VWKJ| 118 ORE => 2 CKRP| 2 LTCQX => 3 XQBHG| 1 GPWVC => 4 SMFQ| 6 CKRP => 4 RCWL| 39 LHZMD, 15 CKFTS, 26 HVBW, 57 KTBRM, 13 DFCM, 30 KZGD, 35 FPNB, 1 LKQCJ, 45 HJGR, 22 RCZS, 34 VWKJ => 1 FUEL| 1 BQPG, 2 BGNQ, 12 WBPD => 8 LTCQX| 2 WSXN => 2 HPXCL| 3 GRFPX => 5 XVLT| 1 LVRSZ => 3 SVMNB| 6 HLMT => 9 ZPTXL| 20 GFSZ => 5 GZNJZ| 1 RCWL => 9 KPNWG| 24 BGNQ, 31 KTBRM => 8 FLFT| 14 VSVG => 9 DBNLM| 191 ORE => 8 CXQB| 115 ORE => 2 SWVLZ| 17 KZRTJ, 13 KPNWG => 7 CKRVN| 9 BQPG => 4 XWLQM| 4 SMFQ, 2 GRFPX => 1 MFJFW| 6 CXQB, 4 CKRP, 2 BXVL, 5 GZNJZ, 3 VWJS, 1 FLFT, 4 KPNWG => 7 DFCM| 1 TBHVH => 6 BGNQ| 3 LQMT => 7 HLMT| 11 GDSDP => 4 WBPD| 2 KPNWG, 5 VWJS, 33 NBRCS => 7 NVDW| 5 GDSDP => 6 FGCMS| 1 GPWVC, 7 BGNQ, 1 FRWGJ => 8 GRFPX| 23 KTBRM, 11 VRMXL, 6 GPWVC => 5 SRJHK| 2 XQBHG, 1 GZNJZ => 3 HVBW| 1 ZTCSK => 4 WSXN| 1 XVLT, 5 HLMT, 1 ZPTXL, 2 HVBW, 7 NVDW, 1 WKGQS, 1 LTCQX, 5 MPZH => 3 FPNB| 16 SRJHK => 6 DWBW| 1 SVMNB, 1 VRMXL => 3 HGXJH| 133 ORE => 6 VSVG| 3 NBRCS, 1 FGCMS => 4 LQMT| 1 CKRP => 4 ZTCSK| 5 CKRVN, 1 FLFT => 1 RCZS| 4 ZTCSK, 15 RCWL => 9 LKQCJ| 1 SWVLZ => 8 NBRCS| 5 CKRP, 14 CXQB => 5 VRMXL| 1 SMFQ, 1 DWBW => 2 LHZMD"

recipies :: Recipie
recipies = let as = splitOn "|" puzzleInput in
           let bs = toTuples (map (splitOn " =>") as) in
           let ings = map (\d -> ((last.tail) (splitOn " " (snd d)), ( read ((head.tail) (splitOn " " (snd d))) :: Integer  , map (\(a,b) -> (read a:: Integer,b)) (toTuples (map (tail . (splitOn " ")) (splitOn "," (fst d))))))) bs in
           Map.fromList ings

toTuples :: [[a]] -> [(a,a)]
toTuples xs = map (\c -> (head c, last c)) xs  

have :: Maybe Integer -> Integer
have (Just x) = x
have (Nothing) = 0

make :: Recipie -> [Ingredient] -> Map.Map Chemical Integer -> Integer
make pies [] made = have (Map.lookup "ORE" made)
make pies (r:reqs) made = let (qty, chem) = r in
                          if chem == "ORE" then 
                            let ore = have (Map.lookup "ORE" made) in
                            make pies reqs (Map.insert "ORE" (ore + qty) made)
                          else
                          let (makes, ings) = fromJust (Map.lookup chem pies) in 
                          let hav = have (Map.lookup chem made) in
                          let need = maximum [qty - hav, 0] in 
                          if need == 0 then make pies reqs (Map.insert chem (hav-qty) made) else
                            let need' = (need `div` makes) + (if need `mod` makes == 0 then 0 else 1) in
                            let waste = (need' * makes) - need in
                            let reqIngs = map (\(a, c) -> (a*need', c)) ings in
                            make pies (reqs ++ reqIngs) (Map.insert chem waste made)

answerA :: Integer
answerA = make recipies [(1, "FUEL")] Map.empty 

--answerA = 362713

reserves :: Integer
reserves = 1000000000000

requires :: Integer -> Integer
requires n = make recipies [(n, "FUEL")] Map.empty

canMake :: Integer -> Integer -> Integer -> Integer
canMake res n ch = let a = requires n in
                   let b = requires (n+1) in
                   if (a<res) && (b>res) then n else
                    let chd = if a<res then 1 else (-1) in
                    canMake res (n+(ch*chd)) (maximum [ch `div` 2, 1])

answerB :: Integer
answerB = canMake reserves reserves (reserves `div` 2)

--answerB = 3281820

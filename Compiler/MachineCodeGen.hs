module MachineCodeGen where

import SemanticAnalyser 
import Parser
import Lexer

data Param = Value Integer | Location Integer | RelLocation Integer
                deriving Show

data MachineCode = Add Param Param Param -- + 4
                 | Times Param Param Param -- + 4
                 | Output Param -- + 2
                 | JumpTrue Param Param -- + 3
                 | JumpFalse Param Param -- + 3
                 | LessThan Param Param Param -- + 4
                 | Equal Param Param Param -- + 4
                deriving Show 


 --                 STACK     POSITION                                  STACK     POSITION
cGenSeq :: CSeq -> Integer -> Integer -> Environment -> ([MachineCode], Integer, Integer) -- integers are available place on stack, from stack indexed at 0, pointer to start location in code

cGenSeq (CS s1 s2) stack position env = let (mc, s, p) = cGenSeq s1 stack position env in
                                        let (mc', s', p') = cGenSeq s2 stack p env in
                                            (mc ++ mc', stack, p')

cGenSeq (CIf e1 s1 s2) stack position env = let (cond, l, p) = cGenExpr e1 stack position env in
                                            let (b1, s, p') = cGenSeq s1 (l+1) (3 + p) env in
                                            let (b2, s', p'') = cGenSeq s2 (l+1) (3 + p') env in
                                            (cond ++ [JumpFalse (RelLocation l) (Value (p' + 3))] ++ b1 ++ [JumpTrue (Value 1) (Value p'')] ++ b2, stack, p'')

cGenSeq (CWhile e1 s1) stack position env = let (cond, s, p) = cGenExpr e1 stack position env in
                                            let (bdy, s', p') = cGenSeq s1 (s + 1) (p + 3) env in 
                                            (cond ++ [JumpFalse (RelLocation s) (Value (p' + 3))] ++ bdy ++ [JumpTrue (Value (1)) (Value (position))], stack, (p' + 3)) 

cGenSeq (CFor e1 e2 s1 s2) stack position env = let (init, s, p) = cGenSeq e1 stack position env in 
                                                let (cond, s', p') = cGenExpr e2 (s + 1) p env in
                                                let (bdy, s'', p'') = cGenSeq s2 (s' + 1) (p' + 3) env in
                                                let (iter, s''', p''') = cGenSeq s1 (s'' + 1) (p'') env in
                                                (init ++ cond ++ [JumpFalse (RelLocation (s')) (Value (p''' + 3))] ++ bdy ++ iter ++ [JumpTrue (Value 1) (Value p)], stack, (p''' + 3))

cGenSeq (CInit (CIdent x)) stack position env = ([Add (Value 0) (Value 0) (RelLocation (findLoc x 0 env))], stack, (position+4))

cGenSeq (CAssign (CIdent x) e1) stack position env = let (e, l, p) = cGenExpr e1 stack position env in 
                                             (e ++ [Add (Value 0) (RelLocation l) (RelLocation (findLoc x 0 env))], stack, (p+4))

cGenSeq (CPrint e1) stack position env = let (e, l, p) = cGenExpr e1 stack position env in 
                                        (e ++ [Output (RelLocation l)], stack, (p + 2))

cGenSeq (CSkip) stack position env = ([], stack, position)


--                   STACK       POSITION                                   STACK   POSITION
cGenExpr :: CExpr -> Integer -> Integer -> Environment -> ([MachineCode], Integer, Integer) -- integer returned is expression result location on stack

cGenExpr (CGt e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                          let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                          (e1' ++ e2' ++ [LessThan (RelLocation s) (RelLocation s') (RelLocation (s' + 1)), Equal (RelLocation s) (RelLocation s') (RelLocation (s' + 2)), Add (RelLocation (s' + 1)) (RelLocation (s' + 2)) (RelLocation (s' + 3)), Equal (RelLocation (s' + 3)) (Value 0) (RelLocation (s' + 4))], (s' + 4), (p' + 16))

cGenExpr (CLt e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                          let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                          (e1' ++ e2' ++ [LessThan (RelLocation s) (RelLocation s') (RelLocation (s' + 1))], (s' + 1), (p' + 4))

cGenExpr (CLte e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                           let (e2', s', p') = cGenExpr e2 (s+1) p env in 
                                           (e1' ++ e2' ++ [LessThan (RelLocation s) (RelLocation s') (RelLocation (s' + 1)), Equal (RelLocation s) (RelLocation s') (RelLocation (s' + 2)), Add (RelLocation (s' + 1)) (RelLocation (s' + 2)) (RelLocation (s' + 3)), LessThan (Value 0) (RelLocation (s' + 3)) (RelLocation (s' + 4))], (s' + 4), (p' + 16))                                          

cGenExpr (CGte e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                           let (e2', s', p') = cGenExpr e2 (s+1) p env in 
                                           (e1' ++ e2' ++ [LessThan (RelLocation s) (RelLocation s') (RelLocation (s' + 1)), Equal (Value 0) (RelLocation (s' + 1)) (RelLocation (s' + 2))], (s' + 2), (p' + 8))

cGenExpr (CEq e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                          let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                          (e1' ++ e2' ++ [Equal (RelLocation s) (RelLocation s') (RelLocation (s' + 1))], (s' + 1), (p' + 4))

cGenExpr (CNeq e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                           let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                           (e1' ++ e2' ++ [Equal (RelLocation s) (RelLocation s') (RelLocation (s' + 1)), Equal (RelLocation (s' + 1)) (Value 0) (RelLocation (s' + 2))], (s' + 2), (p' + 8))

cGenExpr (CPlus e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                            let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                            (e1' ++ e2' ++ [Add (RelLocation s) (RelLocation s') (RelLocation (s' + 1))], (s' + 1), (p' + 4))

cGenExpr (CMult e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                            let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                            (e1' ++ e2' ++ [Times (RelLocation s) (RelLocation s') (RelLocation (s' + 1))], (s' + 1), (p' + 4))

cGenExpr (CDiv  e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                            let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                            (e1' ++ e2' ++ [Add (Value 0) (Value 0) (RelLocation (s' + 1)), Times (RelLocation s') (Value (-1)) (RelLocation (s'+ 2)), LessThan (RelLocation s) (RelLocation s') (RelLocation (s'+3)), JumpTrue (RelLocation (s' + 3)) (Value (p' + 26)), Add (RelLocation s) (RelLocation (s' + 2)) (RelLocation s), Add (RelLocation (s' + 1)) (Value 1) (RelLocation (s' + 1)), JumpTrue (Value 1) (Value (p' + 8))], (s' + 1), (p' + 26))

cGenExpr (CSub e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                           let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                           (e1' ++ e2' ++ [Times (RelLocation s') (Value (-1)) (RelLocation (s' + 1)), Add (RelLocation s) (RelLocation (s' + 1)) (RelLocation (s' + 2))], (s' + 2), (p' + 8))

cGenExpr (COr e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                          let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                          (e1' ++ e2' ++ [Add (RelLocation s') (RelLocation s) (RelLocation (s' + 1)), LessThan (Value 0) (RelLocation (s' + 1)) (RelLocation (s' + 2))], (s' + 2), (p' + 8))

cGenExpr (CAnd e1 e2) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                           let (e2', s', p') = cGenExpr e2 (s+1) p env in
                                           (e1' ++ e2' ++ [Add (RelLocation s') (RelLocation s) (RelLocation (s' + 1)), Equal (Value 2) (RelLocation (s' + 1)) (RelLocation (s' + 2))], (s' + 2), (p' + 8))

cGenExpr (CNot e1) stack position env = let (e1', s, p) = cGenExpr e1 stack position env in 
                                        (e1' ++ [Equal (RelLocation s) (Value 0) (RelLocation (s+1))], (s+1), (p+4))

cGenExpr (CItg n) stack position env = ([Add (Value n) (Value 0) (RelLocation (stack + 1))], (stack + 1), (position + 4))

cGenExpr (CBln n) stack position env = ([Add (Value n) (Value 0) (RelLocation (stack + 1))], (stack + 1), (position + 4))

cGenExpr (CIdent x) stack position env = ([Add (RelLocation (findLoc x 0 env)) (Value 0) (RelLocation (stack + 1))], (stack + 1), (position + 4))

findLoc :: String -> Integer -> Environment -> Integer
findLoc a n [] = error "Compiler Error"
findLoc a n ((x,t):xs)
        | x == a = n
        | otherwise = findLoc a (n+1) xs
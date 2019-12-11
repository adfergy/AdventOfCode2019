module Compiler where

import MachineCodeGen
import SemanticAnalyser 
import Parser
import Lexer


paramMode :: Param -> (String, Integer)
paramMode (Value n) = ("1", n)
paramMode (Location n) = ("0", n)
paramMode (RelLocation n) = ("2", n)

machineCodeToIntCode :: MachineCode -> [Integer]
machineCodeToIntCode (Add p1 p2 p3) = let (m1, p1') = paramMode p1 in
                                      let (m2, p2') = paramMode p2 in
                                      let (m3, p3') = paramMode p3 in
                                      [(read (m3 ++ m2 ++ m1 ++ "01") :: Integer), p1', p2', p3']

machineCodeToIntCode (Times p1 p2 p3) = let (m1, p1') = paramMode p1 in
                                       let (m2, p2') = paramMode p2 in
                                       let (m3, p3') = paramMode p3 in
                                       [(read (m3 ++ m2 ++ m1 ++ "02") :: Integer), p1', p2', p3']

machineCodeToIntCode (Output p1 ) = let (m1, p1') = paramMode p1 in
                                       [(read (m1 ++ "04") :: Integer), p1']

machineCodeToIntCode (JumpTrue p1 p2) = let (m1, p1') = paramMode p1 in
                                       let (m2, p2') = paramMode p2 in
                                       [(read ( m2 ++ m1 ++ "05") :: Integer), p1', p2']

machineCodeToIntCode (JumpFalse p1 p2) = let (m1, p1') = paramMode p1 in
                                       let (m2, p2') = paramMode p2 in
                                       [(read ( m2 ++ m1 ++ "06") :: Integer), p1', p2']

machineCodeToIntCode (LessThan p1 p2 p3) = let (m1, p1') = paramMode p1 in
                                       let (m2, p2') = paramMode p2 in
                                       let (m3, p3') = paramMode p3 in
                                       [(read (m3 ++ m2 ++ m1 ++ "07") :: Integer), p1', p2', p3']

machineCodeToIntCode (Equal p1 p2 p3) = let (m1, p1') = paramMode p1 in
                                       let (m2, p2') = paramMode p2 in
                                       let (m3, p3') = paramMode p3 in
                                       [(read (m3 ++ m2 ++ m1 ++ "08") :: Integer), p1', p2', p3']


compile :: String -> [Integer]
compile s = let lexbuf = parse s 0 in
            let par = genSeq lexbuf in
            let (chk, vars) = checkSeq par [] in
            let (mc, s, end) = cGenSeq chk (toInteger (length vars)) 2 vars in
            [009, (end + 1)] ++ (concat (map machineCodeToIntCode mc)) ++ [99]


main = do 
     putStrLn ("Filename: ")
     filename <- getLine
     s <- readFile filename
     putStrLn (show (compile s))
module Parser where
import Lexer

data Seq =  If Expr Seq Seq
          | While Expr Seq
          | For Seq Expr Seq Seq
          | Init Expr
          | Assign Expr Expr
          | Print Expr
          | Skip
          | S Seq Seq
            deriving Show

data Expr =   Gt Expr Expr
            | Lt Expr Expr
            | Eq Expr Expr
            | Lte Expr Expr
            | Gte Expr Expr
            | Neq Expr Expr
            | Plus Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Sub Expr Expr
            | Or Expr Expr
            | And Expr Expr
            | Not Expr
            | Itg Integer
            | Bln Integer
            | IdentV String
            | IdentB String
            | Ident String
            deriving Show

getLn :: [Token] -> ([Token], [Token])
getLn = find SEMI []

getMatch lpar rpar n (s:ss) 
                    | s == lpar = let (a,b) = getMatch lpar rpar (n+1) ss in 
                                        (s : a, b)
                    | s == rpar = if (n==0) then ([], ss) 
                                  else (let (a,b) = getMatch lpar rpar (n-1) ss in 
                                                               (s : a, b))
                    | otherwise = let (a,b) = getMatch lpar rpar (n) ss in (s : a, b)

find tok rs [] = (rs,[])
find tok rs (s:ss) 
            | s == tok = (rs, ss)
            | otherwise = find tok (rs ++ [s]) ss

genIf :: [Token] -> (Seq, [Token])
genIf (LPAR:ss) =  let (cond, ss') = getMatch LPAR RPAR 0 ss in
                    if (head ss') /= LCURL then error "Syntax Error - No curly brackets"
                    else 
                        let (b1, ss'') = getMatch LCURL RCURL 0 (tail ss') in
                        if ss'' /= [] && head (ss'') == ELSE then
                            if head(tail ss'') /= LCURL then error "Syntax Error - No Brackets around condition" else
                                let (b2, ss''') = getMatch LCURL RCURL 0 (tail (tail ss'')) in
                                  (If (genExpr cond) (genSeq b1) (genSeq b2), ss''')
                        else (If (genExpr cond) (genSeq b1) Skip, ss'')
genIf _ = error "Syntax Error - No Brackets around condition" -- Could be extended to remove parenthesis

genWhile :: [Token] -> (Seq, [Token])
genWhile (LPAR:ss) = let (cond, ss') = getMatch LPAR RPAR 0 ss in
                        if (head ss') /= LCURL then error "Syntax Error"
                        else 
                            let (b1, ss'') = getMatch LCURL RCURL 0 (tail ss') in
                            (While (genExpr cond) (genSeq b1), ss'')
genWhile _ = error "Syntax Error - No Brackets around condition"-- Could be extended to remove parenthesis

genFor :: [Token] -> (Seq, [Token])
genFor (LPAR:ss) = let (guard, ss') = getMatch LPAR RPAR 0 ss in
                   let (v, ss'') = find SEMI [] guard in
                   let (cond, incr) = find SEMI [] ss'' in
                   if (head ss') /= LCURL then error "Syntax Error - No curly brackets"
                    else 
                        let (b1, ss''') = getMatch LCURL RCURL 0 (tail ss') in
                         (For (genSeq v) (genExpr cond) (genSeq incr) (genSeq b1), ss''')
genFor _ = error "Syntax Error - No Brackets around condition"-- Could be extended to remove parenthesis

genPrint :: [Token] -> (Seq, [Token])
genPrint ss = let (print, ss') = getLn ss in
              (Print (genExpr print), ss')

genInit :: Token -> [Token] -> (Seq, [Token])
genInit VAR ((IDENT s):ss) = (Init (IdentV s), tail(ss))
genInit BOOL ((IDENT s):ss) = (Init (IdentB s), tail(ss))
genInit _ _ = error "Syntax Error - Declaring Non Variable"



genSeq :: [Token] -> Seq
genSeq [] = Skip
genSeq (s:ss) 
    | s == IF = let (a,b) = genIf ss in 
                    S a (genSeq b)
    | s == WHILE = let (a,b) = genWhile ss in
                    S a (genSeq b)
    | s == FOR = let (a,b) = genFor ss in
                    S a (genSeq b)
    | s == PRINT = let (a,b) = genPrint ss in
                    S a (genSeq b)
    | s == VAR = let (a,b) = genInit VAR ss in
                    S a (genSeq b)
    | s == BOOL = let (a,b) = genInit BOOL ss in
                    S a (genSeq b)
    | otherwise = let (a,b) = getLn (s:ss) in
                    if (elem ASSIGN a) then 
                        let (c,d) = find ASSIGN [] a in
                        S (Assign (genExpr [head c]) (genExpr d)) (genSeq b)
                    else error "Syntax Error - Unknown"

genExpr :: [Token] -> Expr
genExpr (ss)
    | head ss == NOT = Not (genExpr (tail ss))
    | elem LPAR ss = let (a,ss') = find LPAR [] ss in
                     let (b,c) = getMatch LPAR RPAR 0 ss' in 
                     if a == [] then (if c == [] then genExpr b else combExpr (head c) (genExpr b) (genExpr (tail c))) 
                     else if c == [] then (combExpr (last a) (genExpr (init a)) (genExpr b)) 
                     else if b == [] then error "Syntax Error - Empty Brackets"
                     else combExpr (head c) (combExpr (last a) (genExpr (init a)) (genExpr b)) (genExpr (tail c)) 
    | elem SUB  ss = let (a, b) = find SUB [] ss in
                     Sub (genExpr a) (genExpr b)
    | elem PLUS ss = let (a, b) = find PLUS [] ss in
                     Plus (genExpr a) (genExpr b)
    | elem DIV ss = let (a, b) = find DIV [] ss in
                     Div (genExpr a) (genExpr b)
    | elem MULT ss = let (a, b) = find MULT [] ss in
                     Mult (genExpr a) (genExpr b)
    | elem AND ss = let (a, b) = find AND [] ss in
                     And (genExpr a) (genExpr b)  
    | elem OR ss = let (a, b) = find OR [] ss in
                     Or (genExpr a) (genExpr b)      
    | elem NEQ ss = let (a, b) = find NEQ [] ss in
                     Neq (genExpr a) (genExpr b)
    | elem EQL ss = let (a, b) = find EQL [] ss in
                     Eq (genExpr a) (genExpr b)        
    | elem LTE ss = let (a, b) = find LTE [] ss in
                     Lte (genExpr a) (genExpr b)
    | elem GTE ss = let (a, b) = find GTE [] ss in
                     Gte (genExpr a) (genExpr b)
    | elem LTH ss = let (a, b) = find LTH [] ss in
                     Lt (genExpr a) (genExpr b) 
    | elem GTH ss = let (a, b) = find GTH [] ss in
                     Gt (genExpr a) (genExpr b)   
            
genExpr [(IDENT x)] = Ident x
genExpr [(INT x)] = Itg x
genExpr [(BLN x)] = Bln x
genExpr _ = error "Syntax Error - Bad Expression"

combExpr :: Token -> Expr -> Expr -> Expr
combExpr tok e1 e2 
            | tok == AND = And e1 e2
            | tok == OR = Or e1 e2
            | tok == GTH = Gt e1 e2
            | tok == LTH = Lt e1 e2
            | tok == GTE = Gte e1 e2
            | tok == LTE = Lte e1 e2
            | tok == EQL = Eq e1 e2
            | tok == NEQ = Neq e1 e2
            | tok == MULT = Mult e1 e2
            | tok == DIV = Div e1 e2
            | tok == PLUS = Plus e1 e2
            | tok == SUB = Sub e1 e2

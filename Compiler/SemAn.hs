module SemanticAnalyser where

import Lexer
import Parser

type Environment = [(String, ExprType)]

data CSeq = CIf CExpr CSeq CSeq
          | CWhile CExpr CSeq
          | CFor CSeq CExpr CSeq CSeq
          | CInit CExpr
          | CAssign CExpr CExpr
          | CPrint CExpr
          | CSkip
          | CS CSeq CSeq
            deriving Show

data ExprType = Boolean
              | Numeric
              deriving (Show, Eq)

data CExpr =  CGt CExpr CExpr
            | CLt CExpr CExpr
            | CEq CExpr CExpr
            | CLte CExpr CExpr
            | CGte CExpr CExpr
            | CNeq CExpr CExpr
            | CPlus CExpr CExpr
            | CMult CExpr CExpr
            | CDiv CExpr CExpr
            | CSub CExpr CExpr
            | COr CExpr CExpr
            | CAnd CExpr CExpr
            | CNot CExpr
            | CItg Integer
            | CBln Integer
            | CIdent String
            deriving Show

checkSeq :: Seq -> Environment -> (CSeq, Environment)

checkSeq (S s1 s2) env = let (s1', env') = checkSeq s1 env in
                         let (s2', env'') = checkSeq s2 env' in
                         (CS s1' s2', env'')

checkSeq (If e1 s1 s2) env = let (e1', t) = checkExpr env e1 in
                             if t /= Boolean then error "Sematic Error - Condition in 'if' statement not boolean" else 
                             let (s1', env') = checkSeq s1 env in
                             let (s2', env'') = checkSeq s2 env' in
                             (CIf e1' s1' s2', env'')

checkSeq (While e1 s1) env = let (e1', t) = checkExpr env e1 in
                             if t /= Boolean then error "Sematic Error - Condition in 'while loop not boolean" else 
                             let (s1', env') = checkSeq s1 env in
                             (CWhile e1' s1', env')

checkSeq (For init e1 s1 s2) env = let (ini', env') = checkSeq init env in
                                         let (e1', t) = checkExpr env e1 in
                                         if t /= Boolean then error "Sematic Error - Condition in 'for' loop not boolean" else 
                                         let (s1', env'') = checkSeq s1 env' in
                                         let (s2', env''') = checkSeq s2 env'' in
                                            (CFor ini' e1' s1' s2', env''')

checkSeq (Init (IdentV x)) env = if elem (x, Numeric) env || elem (x, Boolean) env then error ("Semantic Error - Re-Initialising varialble " ++ show x)
                                else (CInit (CIdent x), (x, Numeric):env)

checkSeq (Init (IdentB x)) env = if  elem (x, Numeric) env || elem (x, Boolean) env then error ("Semantic Error - Re-Initialising varialble " ++ show x)
                                else (CInit (CIdent x), (x, Boolean):env)

checkSeq (Assign (Ident x) e1) env = let (e1', t) = checkExpr env e1 in
                                     if not(elem (x, t) env) then error "Semantic Error - Assigning to undeclared varialble" else
                                     (CAssign (CIdent x) e1', env)

checkSeq (Print e1) env = let (e1', t) = checkExpr env e1 in
                           (CPrint e1', env)

checkSeq Skip env = (CSkip, env)

checkExpr :: Environment -> Expr -> (CExpr, ExprType)

checkExpr env (Gt e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                       if t1 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else 
                       if t2 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else
                        (CGt e1' e2', Boolean)

checkExpr env (Lt e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                       if t1 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else 
                       if t2 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else
                        (CLt e1' e2', Boolean)

checkExpr env (Lte e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                       if t1 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else 
                       if t2 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else
                        (CLte e1' e2', Boolean)

checkExpr env (Gte e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                       if t1 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else 
                       if t2 /= Numeric then error "Semantic Error - Order Undefined For Boolean" else
                        (CGte e1' e2', Boolean)

checkExpr env (Eq e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                        (CEq e1' e2', Boolean)

checkExpr env (Neq e1 e2) = let (e1', t1) = checkExpr env e1 in
                       let (e2', t2) = checkExpr env e2 in
                        (CNeq e1' e2', Boolean)

checkExpr env (Plus e1 e2) = let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Numeric then error "Semantic Error - Addition Undefined For Boolean" else 
                         if t2 /= Numeric then error "Semantic Error - Addition Undefined For Boolean" else
                            (CPlus e1' e2', Numeric)

checkExpr env (Mult e1 e2) = let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Numeric then error "Semantic Error - Multiplication Undefined For Boolean" else 
                         if t2 /= Numeric then error "Semantic Error - Multiplication Undefined For Boolean" else
                            (CMult e1' e2', Numeric)

checkExpr env (Div e1 e2) = let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Numeric then error "Semantic Error - Division Undefined For Boolean" else 
                         if t2 /= Numeric then error "Semantic Error - Division Undefined For Boolean" else
                            (CDiv e1' e2', Numeric)

checkExpr env (Sub e1 e2) = let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Numeric then error "Semantic Error - Subtraction Undefined For Boolean" else 
                         if t2 /= Numeric then error "Semantic Error - Subtraction Undefined For Boolean" else
                            (CSub e1' e2', Numeric)

checkExpr env (Or e1 e2 ) =  let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Boolean then error "Semantic Error - Logic Undefined For Numeric Values" else 
                         if t2 /= Boolean then error "Semantic Error - Logic Undefined For Numeric Values" else
                            (COr e1' e2', Boolean)

checkExpr env (And e1 e2) =  let (e1', t1) = checkExpr env e1 in
                         let (e2', t2) = checkExpr env e2 in
                         if t1 /= Boolean then error "Semantic Error - Logic Undefined For Numeric Values" else 
                         if t2 /= Boolean then error "Semantic Error - Logic Undefined For Numeric Values" else
                            (CAnd e1' e2', Boolean)

checkExpr env (Not e1) =     let (e1', t1) = checkExpr env e1 in
                         if t1 /= Boolean then error "Semantic Error - Logic Undefined For Numeric Values" else 
                            (CNot e1', Boolean)

checkExpr env (Itg n) = (CItg n, Numeric)

checkExpr env (Bln n) = (CBln n, Boolean)

checkExpr env (Ident x) = let t = findType x env in
                            case t of
                                Just(et) -> (CIdent x, et)
                                Nothing  -> error ("Semantic Error - undeclared variable " ++ show x)

findType x [] = Nothing
findType x ((s,t):xs) 
                | s == x = Just (t)
                | otherwise = findType x xs 
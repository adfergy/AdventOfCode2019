module Lexer where

data Token = VAR | BOOL | IF | ELSE | WHILE | FOR | SEMI | LCURL | RCURL | GTH | LTH | LTE | GTE | EQL | NEQ | ASSIGN | LPAR | RPAR | PLUS | MULT | DIV | SUB | OR | AND | FALSE | NOT | PRINT | IDENT String | BLN Integer | INT Integer
    deriving (Show, Eq)

parse :: String -> Int -> [Token]
parse "" l = []
parse ('/':'/':ss) l = parse (dropWhile ('\n'/=) ss) l
parse ('v':'a':'r':ss) l = VAR : parse ss l
parse ('b':'o':'o':'l':ss) l = BOOL : parse ss l
parse ('w':'h':'i':'l':'e':ss) l = WHILE : parse ss l
parse ('f':'o':'r':ss) l = FOR : parse ss l
parse ('i':'f':ss) l = IF : parse ss l
parse ('e':'l':'s':'e':ss) l = ELSE : parse ss l
parse ('p':'r':'i':'n':'t':ss) l = PRINT : parse ss l
parse ('t':'r':'u':'e':ss) l = (BLN 1) : parse ss l
parse ('f':'a':'l':'s':'e':ss) l = (BLN 0) : parse ss l
parse (';':ss) l = SEMI : parse ss l
parse ('{':ss) l = LCURL : parse ss l
parse ('}':ss) l = RCURL : parse ss l
parse ('>':'=':ss) l = GTE : parse ss l
parse ('<':'=':ss) l = LTE : parse ss l
parse ('>':ss) l = GTH : parse ss l
parse ('<':ss) l = LTH : parse ss l
parse ('=':'=':ss) l = EQL : parse ss l
parse ('!':'=':ss) l = NEQ : parse ss l
parse (':':'=':ss) l = ASSIGN : parse ss l
parse ('(':ss) l = LPAR : parse ss l
parse (')':ss) l = RPAR : parse ss l
parse ('+':ss) l = PLUS : parse ss l
parse ('*':ss) l = MULT : parse ss l
parse ('/':ss) l = DIV : parse ss l
parse ('-':ss) l = SUB : parse ss l
parse ('|':'|':ss) l = OR : parse ss l
parse ('&':'&':ss) l = AND : parse ss l
parse ('!':ss) l = NOT : parse ss l
parse (' ':ss) l = parse ss l
parse ('\n':ss) l = parse ss (l+1)
parse (x:ss) l = if elem x (['a'..'z']++['0'..'9']) then
               let var = x : takeWhile (\c -> elem c chars) ss in
               (if isInteger var then INT (read var :: Integer) else IDENT var) : parse (dropWhile (\c -> elem c chars) ss)  l
               else error ("Parse Error On Line " ++ show l)


isInteger :: String -> Bool
isInteger ""  = False
isInteger xs  =
  case dropWhile isDigit xs of
    ""       -> True
    _        -> False

isDigit x = elem x ['0'..'9']

chars :: [Char]
chars =  ['a'..'z']++['A'..'Z']++['0'..'9']
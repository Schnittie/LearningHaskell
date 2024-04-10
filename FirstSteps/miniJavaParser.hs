module Lexer (Token(..),lexer) where

import Data.Char




type Parser tok a = [tok] -> [(a,[tok])]

failure :: Parser tok b
failure = \a -> []

succeed :: a -> Parser tok a
succeed value toks = [(value, toks)]

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy cond [] = []
satisfy cond (tok : toks) | cond tok = succeed tok toks
                          | otherwise = failure toks

(+.+) :: Parser tok a -> Parser tok b -> Parser tok (a,b)
(p1 +.+ p2) toks = [((v1,v2),rest2) | (v1, rest1) <- p1 toks, (v2, rest2) <- p2 rest1]

(|||) :: Parser tok a -> Parser tok a -> Parser tok a
(p1 ||| p2) toks = p1 toks ++ p2 toks

(<<<) :: Parser tok a -> (a -> b) -> Parser tok b
(p <<< f) toks = [ (f v, rest) | (v, rest) <- p toks]




data Token
     = IDENTIFIER String

     | LBRACKET
     | RBRACKET
     | LBRACE
     | RBRACE
     | DOT
     | SEMICOLON
     | COMMA
     | PLUS
     | MINUS
     | MUL
     | DIV
     | LESS
     | GREATER

     | PUBLIC
     | PRIVATE

     | CLASS
     | THIS

     | ASSIGN
     | NEW

     | CHAR
     | VOID
     | BOOLEAN
     | INT
     | STRING

     | IF
     | WHILE
     | ELSE

     | RETURN

     | EQUAL
     | NOTEQUAL
     | PLUSEQUAL
     | MINUSEQUAL
     | LESSEQUAL
     | GREATEREQUAL

     | INCREMENT
     | DECREMENT

     | AND
     | OR

     | BOOLLITERAL Bool
     | CHARLITERAL Char
     | STRINGLITERAL String
     | INTLITERAL Integer
     | JNULL

     deriving (Eq,Show)


lexer :: String -> [Token]
lexer [] = []
lexer ('\'':ch: '\'':cs) = (CHARLITERAL ch) : (lexer cs)
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
      | (c == '"') = lexStr (cs)

lexer ('!':'=':cs) = NOTEQUAL : lexer cs
lexer ('+':'=':cs) = PLUSEQUAL : lexer cs
lexer ('-':'=':cs) = MINUSEQUAL : lexer cs
lexer ('<':'=':cs) = LESSEQUAL : lexer cs
lexer ('>':'=':cs) = GREATEREQUAL : lexer cs
lexer ('+':'+':cs) = INCREMENT : lexer cs
lexer ('-':'-':cs) = DECREMENT : lexer cs
lexer ('=':'=':cs) = EQUAL : lexer cs

lexer ('{':cs) = LBRACKET : lexer cs
lexer ('}':cs) = RBRACKET : lexer cs
lexer ('(':cs) = LBRACE : lexer cs
lexer (')':cs) = RBRACE : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (';':cs) = SEMICOLON : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = MUL : lexer cs
lexer ('/':cs) = DIV : lexer cs
lexer ('<':cs) = LESS : lexer cs
lexer ('>':cs) = GREATER : lexer cs
lexer ('&':cs) = AND : lexer cs
lexer ('|':cs) = OR : lexer cs
lexer ('=':cs) = ASSIGN : lexer cs

lexNum cs = INTLITERAL (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexStr cs = STRINGLITERAL (read str) : lexer rest
       where (str,rest) = span ((/=) '"') cs

lexVar cs =
   case span isAlphaNum  cs of

      ("true",rest) -> BOOLLITERAL True : lexer rest
      ("false",rest) -> BOOLLITERAL False : lexer rest

      ("public",rest) -> PUBLIC : lexer rest
      ("private",rest) -> PRIVATE : lexer rest

      ("class",rest) -> CLASS : lexer rest
      ("this",rest) -> THIS : lexer rest

      ("new",rest) -> NEW : lexer rest

      ("string",rest) -> STRING : lexer rest
      ("char",rest) -> CHAR : lexer rest
      ("void",rest) -> VOID : lexer rest
      ("boolean",rest) -> BOOLEAN : lexer rest
      ("int",rest) -> INT : lexer rest

      ("if",rest) -> IF : lexer rest
      ("while",rest) -> WHILE : lexer rest
      ("else",rest) -> ELSE : lexer rest

      ("return",rest) -> RETURN : lexer rest

      ("null",rest) -> JNULL : lexer rest

      (var,rest)   -> IDENTIFIER var : lexer rest






isIdentifier (IDENTIFIER _) = True
isIdentifier _ = False

isBool (BOOLLITERAL _) = True
isBool _ = False

isChar (CHARLITERAL _) = True
isChar _ = False

isString (STRINGLITERAL _) = True
isString _ = False

isInt (INTLITERAL _) = True
isInt _ = False

java :: Parser Token Bool
java = (classdef +.+ (satisfy((==) LBRACKET)) +.+ classcont) <<< (\(_, _) -> True)

classdef:: Parser Token Bool
classdef = (access +.+ (satisfy((==) CLASS) +.+ (satisfy(isIdentifier)))) <<< (\(_,(_,_)) -> True )


access :: Parser Token Bool
access = (satisfy((==) PUBLIC) <<< (\(_) -> True))
         ||| (satisfy((==) PRIVATE) <<< (\(_) -> True))

classcont :: Parser Token Bool
classcont = ((satisfy((==) RBRACKET)) <<< (\(_) -> True))
            ||| ((meth +.+ classcont) <<< (\(_,_) -> True))
            ||| ((globVar +.+ classcont) <<< (\(_,_) -> True))

globVar :: Parser Token Bool
globVar = ((access +.+ (var +.+ (assignement +.+ (satisfy((==) SEMICOLON))))) <<< (\(_,(_,(_,_))) -> True ))

var :: Parser Token Bool
var = ((varType +.+ (satisfy(isIdentifier)) <<< (\(_, _) -> True)))

varType :: Parser Token Bool
varType = ((satisfy((==) CHAR)) <<< (\(_) -> True))
          ||| ((satisfy((==) BOOLEAN)) <<< (\(_) -> True))
          ||| ((satisfy((==) INT)) <<< (\(_) -> True))
          ||| ((satisfy((==) STRING)) <<< (\(_) -> True))

assignement :: Parser Token Bool
assignement = (((satisfy((==) ASSIGN)) +.+ literal) <<< (\(_, _) -> True))

literal :: Parser Token Bool
literal = (satisfy(isBool) <<< (\(_) -> True))
          ||| (satisfy(isChar) <<< (\(_) -> True))
          ||| (satisfy(isString) <<< (\(_) -> True))
          ||| (satisfy(isInt) <<< (\(_) -> True))

meth :: Parser Token Bool
meth = ((methhead +.+ ((satisfy((==) LBRACKET)) +.+ (methbody +.+ satisfy((==) RBRACKET)))) <<< (\(_,(_,(_,_))) -> True ))

methhead :: Parser Token Bool
methhead = ((access +.+ (methtype +.+ ((satisfy(isIdentifier)) +.+ ((satisfy((==) LBRACE)) +.+ (methparams +.+ ((satisfy((==) RBRACE)))))))) <<< (\(_,(_,(_,(_,(_,_))))) -> True ) )

methtype :: Parser Token Bool
methtype = ((satisfy((==) VOID)) <<< (\(_) -> True))
           ||| (varType <<< (\(_) -> True))

methparams :: Parser Token Bool
methparams = (multipleParams <<< (\(_) -> True))
          ||| (var <<< (\(_) -> True))
          ||| (succeed True)

multipleParams :: Parser Token Bool
multipleParams = ((var +.+ ((satisfy((==) COMMA)) +.+ multipleParams))  <<< (\(_,(_,_)) -> True ))
                 ||| (var <<< (\(_) -> True))

methbody :: Parser Token Bool
methbody = ((jReturn +.+ (satisfy((==) SEMICOLON))) <<< (\(_,_) -> True ))
            ||| (( varassign +.+ ((satisfy((==) SEMICOLON)) +.+ methbody) ) <<< (\(_,(_,_)) -> True ))
            ||| (( expression +.+ ((satisfy((==) SEMICOLON)) +.+ methbody) ) <<< (\(_,(_,_)) -> True ))
            ||| ((methcall +.+ ((satisfy((==) SEMICOLON)) +.+ methbody) ) <<< (\(_,(_,_)) -> True ))
            ||| ((var +.+ ((satisfy((==) SEMICOLON)) +.+ methbody )) <<< (\(_,(_,_)) -> True ))
            ||| (statement +.+ methbody <<< (\(_,_) -> True))
            ||| succeed True

statement :: Parser Token Bool
statement = whileState <<< (\(_) -> True)
            ||| ifState <<< (\(_) -> True)
            ||| (block <<< (\(_) -> True))

whileState :: Parser Token Bool
whileState = ((satisfy((==) WHILE) +.+ (condition +.+ block)) <<< (\(_,(_,_)) -> True ))

ifState :: Parser Token Bool
ifState = ((satisfy((==) IF) +.+ (condition +.+ (block +.+ ((satisfy((==) ELSE)) +.+ block)))) <<< (\(_,(_,_)) -> True ))
          ||| ((satisfy((==) IF) +.+ (condition +.+ block)) <<< (\(_,(_,_)) -> True ))

block :: Parser Token Bool
block = (((satisfy((==) LBRACKET)) +.+ (methbody +.+ (satisfy((==) RBRACKET)))) <<< (\(_,(_,_)) -> True ))

condition :: Parser Token Bool
condition = ((satisfy((==) LBRACE)) +.+ (expression +.+ (satisfy((==) RBRACE))) <<< (\(_,(_,_)) -> True ))

varassign :: Parser Token Bool
varassign = ((var +.+ ((satisfy((==) ASSIGN)) +.+ expression)) <<< (\(_,(_,_)) -> True ))
            ||| ((satisfy(isIdentifier) +.+ ((satisfy((==) ASSIGN)) +.+ expression)) <<< (\(_,(_,_)) -> True ))

expression :: Parser Token Bool
expression = ((exprEnd +.+ (operation +.+ expression))  <<< (\(_,(_,_)) -> True ))
              ||| (((satisfy((==) LBRACE)) +.+ (expression +.+ (satisfy((==) RBRACE)))) <<< (\(_,(_,_)) -> True ))
              ||| (exprEnd <<< (\(_) -> True))

exprEnd :: Parser Token Bool
exprEnd = (methcall <<< (\(_) -> True))
          ||| (var <<< (\(_) -> True))
          ||| ((satisfy(isIdentifier)) <<< (\(_) -> True))
          ||| (literal <<< (\(_) -> True))

operation :: Parser Token Bool
operation = ((satisfy((==) PLUS))  <<< (\(_) -> True))
            ||| ((satisfy((==) MINUS)) <<< (\(_) -> True))
            ||| ((satisfy((==) MUL)) <<< (\(_) -> True))
            ||| ((satisfy((==) DIV)) <<< (\(_) -> True))
            ||| ((satisfy((==) LESS)) <<< (\(_) -> True))
            ||| ((satisfy((==) GREATER)) <<< (\(_) -> True))
            ||| ((satisfy((==) EQUAL)) <<< (\(_) -> True))
            ||| ((satisfy((==) NOTEQUAL)) <<< (\(_) -> True))
            ||| ((satisfy((==) LESSEQUAL)) <<< (\(_) -> True))
            ||| ((satisfy((==) GREATEREQUAL)) <<< (\(_) -> True))

methcall :: Parser Token Bool
methcall = ((satisfy(isIdentifier) +.+ ((satisfy((==) LBRACKET)) +.+ (methargs +.+ (satisfy((==) RBRACKET))))) <<< (\(_,(_,(_,_))) -> True ))

methargs :: Parser Token Bool
methargs = (multipleExpressions <<< (\(_) -> True))
          ||| (expression <<< (\(_) -> True))
          ||| (succeed True)

multipleExpressions :: Parser Token Bool
multipleExpressions = ((expression +.+ (satisfy((==) COMMA)) +.+ multipleExpressions)  <<< (\(x) -> True))
                      ||| (expression <<< (\(_) -> True))

jReturn :: Parser Token Bool
jReturn = ((satisfy((==) RETURN)) +.+ expression) <<< (\(_,_) -> True )
         ||| ((satisfy((==) RETURN)) <<< (\(_) -> True))





correctSolutions :: [(t, [a])] -> [(t, [a])]
correctSolutions sols = (filter (\(_, resttokens) -> null resttokens)) sols

parser :: String -> Bool
parser = fst . head . correctSolutions. java . lexer


main = do
    input <- readFile "test.java"
    print $ lexer input
    s <- readFile "test.java"
    print (parser s)
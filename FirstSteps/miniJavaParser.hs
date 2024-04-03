module Lexer (Token(..),lexer) where

import Data.Char




type Parser tok a = [tok] -> [(a,[tok])]

failure :: Parser a b
failure _ = []

succeed :: a -> Parser tok a
succeed value toks = [(value, toks)]

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy cond [] = []
satisfy cond (tok : toks) | cond tok = succeed tok toks
                          | otherwise = failure toks

(+.+) :: Parser tok a -> Parser tok b -> Parser tok (a,b)
(p1 +.+ p2) toks = [((v1,v2),rest) | (v1, rest1) <- p1 toks, (v2, rest2) <- p2 rest 1]

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


data MiniJava =
       ClassDef
    | Access
    | ClassCont
    | GlobVar
    | Var
    | VarType
    | Assignement
    | Literal
    | Meth
    | MethHead
    | MethBody
    | MethCall
    | MethArgs
    | JReturn
    | Operation
    | Expression
    | VarAssign
    | MultipleExpressions
    | Token
   deriving (Eq,Show)





java :: Parser Token MiniJava
java = classdef +.+ (satisfy((==) LBRACKET)) +.+ classcont

classdef:: Parser Token MiniJava
classdef = access +.+ (satisfy((==) CLASS)) +.+ (satisfy((==) IDENTIFIER))

access :: Parser Token MiniJava
access = (satisfy((==) PUBLIC)) ||| (satisfy((==) PRIVATE))

classcont :: Parser Token MiniJava
classcont = globVar +.+ classcont ||| meth +.+ classcont ||| (satisfy((==) RBRACKET))

globVar :: Parser Token MiniJava
globVar = access +.+ var +.+ assignement +.+ (satisfy((==) SEMICOLON))

var :: Parser Token MiniJava
var = varType +.+ (satisfy((==) IDENTIFIER))

varType :: Parser Token MiniJava
varType = (satisfy((==) CHAR)) ||| (satisfy((==) BOOLEAN)) ||| (satisfy((==) INT)) ||| (satisfy((==) STRING))

assignement :: Parser Token MiniJava
assignement = (satisfy((==) ASSIGN)) +.+ literal

literal :: Parser Token MiniJava
literal = (satisfy((==) BOOLLITERAL)) ||| (satisfy((==) CHARLITERAL)) ||| (satisfy((==) STRINGLITERAL)) ||| (satisfy((==) INTLITERAL))

meth :: Parser Token MiniJava
meth = methhead +.+ (satisfy((==) LBRACKET)) +.+ methbody +.+ (satisfy((==) RBRACKET))

methhead :: Parser Token MiniJava
methhead = access +.+ varType +.+ (satisfy((==) IDENTIFIER))

methbody :: Parser Token MiniJava
methbody = var +.+ (satisfy((==) SEMICOLON)) +.+ methbody ||| varassign +.+ (satisfy((==) SEMICOLON)) +.+ methbody ||| expression +.+ (satisfy((==) SEMICOLON)) +.+ methbody ||| methcall +.+ (satisfy((==) SEMICOLON)) +.+ methbody ||| jReturn +.+ (satisfy((==) SEMICOLON))

varassign :: Parser Token MiniJava
varassign = var +.+ (satisfy((==) ASSIGN)) +.+ expression

expression :: Parser Token MiniJava
expression = expression +.+ operation +.+ expression ||| (satisfy((==) LBRACE)) +.+ expression +.+ (satisfy((==) RBRACE)) ||| methcall ||| var ||| literal

operation :: Parser Token MiniJava
operation = (satisfy((==) PLUS)) ||| (satisfy((==) MINUS)) ||| (satisfy((==) MUL)) ||| (satisfy((==) DIV)) ||| (satisfy((==) LESS)) ||| (satisfy((==) GREATER)) ||| (satisfy((==) EQUAL)) ||| (satisfy((==) NOTEQUAL)) ||| (satisfy((==) LESSEQUAL)) ||| (satisfy((==) GREATEREQUAL))

methcall :: Parser Token MiniJava
methcall = (satisfy((==) IDENTIFIER)) +.+ (satisfy((==) LBRACKET)) +.+ methargs +.+ (satisfy((==) RBRACKET))

methargs :: Parser Token (Maybe MiniJava)
methargs = multipleExpressions ||| expression ||| succeed

multipleExpressions :: Parser Token MiniJava
multipleExpressions = expression +.+ (satisfy((==) COMMA)) +.+ multipleExpressions ||| expression

jReturn :: Parser Token MiniJava
jReturn = (satisfy((==) RETURN)) +.+ expression

parser :: String -> Bool
parser s = case java (lexer s) of
    [(a,[])] -> True
    _ -> False





main = do
    input <- readFile "test.java"
    print $ lexer input
    s <- readFile "test.java"
    print (parser s)
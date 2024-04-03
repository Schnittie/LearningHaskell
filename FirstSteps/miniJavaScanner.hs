module Lexer (Token(..),lexer) where

import Data.Char

data Token
     = IDENTIFIER String
     | INTLITERAL Integer

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
     | PROTECTED
     | PRIVATE
     | STATIC
     | ABSTRACT

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
     | JNULL

     |LOGICALOR
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
      ("protected",rest) -> PROTECTED : lexer rest
      ("private",rest) -> PRIVATE : lexer rest
      ("static",rest) -> STATIC : lexer rest
      ("abstract",rest) -> ABSTRACT : lexer rest

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

main = do
    input <- readFile "test.java"
    print $ lexer input

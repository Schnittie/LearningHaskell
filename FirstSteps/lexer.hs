module Parser (Parser(..)) where


type Parser tok a = [tok] -> [(a,[tok])]

failure :: Parser a b
failure = _ -> []

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
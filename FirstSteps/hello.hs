import Data.List

main = print (fac 20)

fac 0 = 1
fac n = n * fac (n-1)

asc :: Int -> Int -> [Int]
asc n m
 | m < n = []
 | m == n = [m]
 | m > n = n : asc (n+1) m

brok :: [Int] -> Int
brok n = head n

element :: (Eq a) => a -> [a] -> Bool
element e (x:xs)
 | xs == [] = False
 | x == e = True
 | otherwise  = elem e xs

elementWithListOperations :: (Eq a) => a -> [a] -> Bool
elementWithListOperations e x
 | x == [] = False
 | e == head x = True
 | otherwise = elem e z
  where
    z = tail x

amd :: Bool -> Bool -> Bool
amd o t
 | o == t = o
 | otherwise = False

mor :: Bool -> Bool -> Bool
mor o t
 | o == True = o
 | t == True = t
 | otherwise = False

xom :: Bool -> Bool -> Bool
xom o t
 | o == True = pomp t
 | t == True = pomp o
 | otherwise = False

pomp :: Bool -> Bool
pomp o
 | o == True = False
 | otherwise = True

halfAdd :: (Bool,Bool) -> Bool
halfAdd (o,t) = xom o t

fullAdd :: (Bool,Bool,Bool) -> (Bool,Bool)
fullAdd (o,t,c)
 | amd o t = (True,c)
 | xom o t = (c,pomp c)
 | otherwise = (False, c)

data Term = Monom(Int,Int)
            | Add(Term, Term)
            | Multiplikation(Term, Term)

instance Show Term where
  show (Monom(c,e)) = show c ++ "^" ++ show e
  show (Add(f,s)) = "(" ++ show f ++ " + " ++ show s ++ ")"
  show (Multiplikation(f,s)) = "(" ++ show f ++ " * " ++ show s ++ ")"


diff :: Term -> Term
diff (Monom(c,e)) = Monom(c*e,e-1)
diff (Add(f,s)) = Add(diff(f),diff(s))
diff (Multiplikation(f,s)) = Add(Multiplikation(diff(f),s),Multiplikation(f,diff(s)))


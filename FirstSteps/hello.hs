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
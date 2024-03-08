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


main :: IO()
main = do
  print (double 5)


double :: Int -> Int
double n = n*2
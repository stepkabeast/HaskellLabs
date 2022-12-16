main = do 
  print (insertAt 21 [31,122,3] 2)

insertAt :: a -> [a] -> Int -> [a]
insertAt x array n
    | null array = [x]
    | n <= 1 = x : array
    | otherwise = head array : insertAt x (tail array) (n - 1)
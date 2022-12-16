main = putStr (show (duplicat 3 [1,2,3]))

duplicat :: Int -> [a] -> [a]
duplicat _ [] = []
duplicat n (x:xs) = (take n (cycle [x])) ++ duplicat n xs
duplicat :: [array] -> [array]
duplicat [] = []
duplicat (x:xs) = x : x : duplicat xs

main = do 
     putStr (show (duplicat [201,355,456]))
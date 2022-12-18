main = do
   putStrLn(show(myLength [1, 2, 3, 4]))
   putStrLn(show(myLength ['x', 'y', 'z']))

myLength :: [a] -> Int
myLength = foldr (const (+1)) 0
main = do
    putStrLn(show(last' [1, 2, 3, 4]))
    putStrLn(show(last' ['x', 'y', 'z']))

last' :: [a] -> a
last' = foldl1 (const id)
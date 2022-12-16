myLast :: [a] -> a
myLast lst = last(lst)

main = do 
    putStrLn(show(myLast [1, 2, 3, 4]))
    putStrLn(show(myLast ['x', 'y', 'z']))





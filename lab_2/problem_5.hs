myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:t) = myReverse(t) ++ [x]
main = do
    print(myReverse [1,2,3,4])
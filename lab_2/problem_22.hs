main = do 
  print (sequenceOutput 10 30)

sequenceOutput :: Int -> Int -> [Int]
sequenceOutput a b
    | a > b = []
    | otherwise = a : sequenceOutput (a + 1) b
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

main = do
  putStrLn(show(reverse' [1, 2, 3, 4]))
  putStrLn(show(reverse' ['x', 'y', 'z']))
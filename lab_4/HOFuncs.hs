-- функция filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x: xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

-- функция flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- функция map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- функция zipWith
zipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--функция reverse
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
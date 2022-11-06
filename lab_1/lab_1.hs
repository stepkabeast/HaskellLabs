module Main (main) where

data Answer = TwoRoots {x1, x2 :: Double} | OneRoot {x :: Double} | NoRoots

printAnswer :: Answer -> String
printAnswer NoRoots = "No Solutions"
printAnswer (OneRoot x) = "X is " ++ show x
printAnswer (TwoRoots x1 x2) ="X1 is " ++ show x1 ++ "; X2 is " ++ show x2

solve :: Double -> Double -> Double -> Answer
solve a b c
    | a == 0 || discriminant < 0 = NoRoots
    | discriminant > 0 = TwoRoots {x1 = x1, x2 = x2}   
    | otherwise = OneRoot {x = x1}
    where
        discriminant = b^2 - 4 * a * c
        x1 = (-b - sqrt discriminant) / (2 * a)
        x2 = (-b + sqrt discriminant) / (2 * a)

main :: IO ()
main = do 
    print "Enter coefficient a"
    a <- getLine
    print "Enter coefficient b"
    b <- getLine
    print "Enter coefficient c"
    c <- getLine
    print "Answer is"
    print . printAnswer $ solve (read a :: Double) (read b :: Double) (read c :: Double)

data Tree = Branch Char Tree Tree | Empty deriving Show

isEquals :: Tree -> Tree -> Bool
isEquals Empty Empty = True
isEquals Empty _ = False
isEquals _ Empty = False



isEquals (Branch x (sub11) (sub12)) (Branch y (sub21) (sub22)) = (isEquals sub11 sub21) && (isEquals sub12 sub22)

mirror :: Tree -> Tree
mirror Empty = Empty
mirror (Branch x (sub1) (sub2)) = Branch x (mirror sub2) (mirror sub1)

symmetric :: Tree -> Bool
symmetric tree = isEquals tree (mirror tree)

main = do
    print $ show (symmetric (Branch 'x' (Branch 'y' Empty Empty) (Branch 'y' Empty Empty)))
    
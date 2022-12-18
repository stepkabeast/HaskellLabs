data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

tree1 = Branch 1 (Branch 2 Empty (Branch 3 Empty Empty))
                 (Branch 2 Empty Empty)

tree2 = Branch 1 Empty Empty
                 
                
main =  do 
  print (count tree1)
  print (count tree2)
  

count :: Tree a -> Int
count Empty = 0
count (Branch x Empty Empty) = 1
count (Branch x t1 t2) = count t1 + count t2
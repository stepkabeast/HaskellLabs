main = do
  print (paths 1 5  [(1,2),(2,3),(1,3),(3,4),(4,5)])

paths :: Eq a => a -> a -> [(a, a)] -> [[a]]
paths q w e 
    | q == w = [[w]]
    | otherwise = [
        q:path | edge<-e, (fst edge) == q,
        path<-(paths (snd edge) w [e|e<-e, e/=edge])];
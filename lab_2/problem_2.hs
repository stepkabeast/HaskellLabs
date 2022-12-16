func ::  (Show a) => [a] -> a
func [a,b] = a 
func [a] = error "There is only one element"
func [] = error "The list is empty!"

func (a : z) = func z
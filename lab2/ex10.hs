f' :: Num t => t -> t -> (t,t) -> t        

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs) = x || or' xs 

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False 
elem' n (x:xs) = if n == x then True 
                           else elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = [] 
doubleAll (x:xs) = (2*x):doubleAll(xs)

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 1 then (x):selectEven(xs)
                                      else selectEven(xs)

           
                                      


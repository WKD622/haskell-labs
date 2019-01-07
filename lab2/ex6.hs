fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False 
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = True 
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then elem' a xs || True 
                           else elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = (2*x):doubleAll(xs)

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = (x^2):squareAll(xs)

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then (x):selectEven(xs)
                                      else selectEven(xs)

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a 
prod'2 xs = loop 1 xs
 where loop acc []      = acc
       loop acc(x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int 
length'2 xs = loop 0 xs 
 where loop acc []      = acc
       loop acc(x:xs)   = loop (1 + acc) xs

sumABS :: (Num a, Ord a) => [a] -> a
sumABS [] = 0
sumABS (x:xs) = if (x > 0) then x + sumABS xs
                         else x*(-1) + sumABS xs

sumABS' :: (Num a, Ord a) => [a] -> a
sumABS' xs = loop 0 xs 
 where loop acc [] = acc
       loop acc (x:xs) =  if (x > 0) then loop (acc + x) xs
                           else loop (x*(-1) + acc) xs


sumABS2 :: (Num a) => [a] -> a
sumABS2 xs = loop 0 xs 
 where loop acc [] = acc 
       loop acc (x:xs) = loop (abs x + acc) xs

prod3 :: (Num a) => [a] -> a
prod3 xs = loop 1 xs 
      where loop acc [] = acc
            loop (x:xs) = loop (x * acc) xs
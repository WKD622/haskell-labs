import Data.Char

onlyEven [] = [] 
onlyEven (x:xs) = if x `mod` 2 == 1 then onlyEven(xs)
                                    else (x):onlyEven(xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x : filter' p xs 
                          else filter' p xs

                          
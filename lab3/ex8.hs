import Data.Char

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map':: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f(x:xs) = f x : map f xs
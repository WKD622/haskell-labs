import Data.List
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

doFun = do
    putStrLn "Podaj imie: "
    s <- getLine
    putStrLn $ "Witaj " ++ s

fun = putStrLn "Podaj imie: "
    >> getLine
    >>= \line -> putStrLn $ "Witaj " ++ line ++ "!"

instance Foldable Tree where
    foldr f z Empty = z
    foldr f z (Leaf x) = f x z
    foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

dolength = do
    imie <- getLine
    nazwisko <- getLine
    print(length imie + length nazwisko)

dolength2 = putStrLn "PODAJ IMIE: " 
          >> getLine 
          >>= \line1 -> putStrLn "PODAJ NAZWISKO:"
          >> getLine
          >>= \line2 -> print $ ( length line1 + length line2)  
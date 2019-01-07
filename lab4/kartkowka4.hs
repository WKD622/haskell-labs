data Mytype = C1 (Bool, Int) | C2 Int | C3 Double

instance Eq Mytype where
    (==) (C1 (b1,i1)) (C1 (b2,i2)) = b1 == b2 && i1 == i2
    (==) (C2 i1) (C2 i2) = i1 == i2
    (==) (C3 d1) (C3 d2) = d1 == d2

----------------------------------------

data Tree a = Node [Tree a] a | Leaf a

maxValue :: Ord a -> Tree a -> a
maxValue Node (x:xs) n = if n > 

data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int 
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

-------------------------------------

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving Show 

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

-------------------------------------

data Expr a = Lit a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Multiply (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

-------------------------------------

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + depthOfBT lt + depthOfBT rt

-------------------------------------

flattenBTpre :: BinTree a -> [a]
flattenBTpre EmptyBT = []
flattenBTpre (NodeBT n lt rt) = [] ++ [n] ++ flattenBTpre lt ++ flattenBTpre rt

-------------------------------------

flattenBTin :: BinTree a -> [a]
flattenBTin EmptyBT = []
flattenBTin (NodeBT n lt rt) = [] ++ flattenBTin lt ++ [n] ++ flattenBTin rt

-------------------------------------

flattenBTpost :: BinTree a -> [a]
flattenBTpost EmptyBT = []
flattenBTpost (NodeBT n lt rt) = [] ++ flattenBTpost lt ++ flattenBTpost rt ++ [n] 

-------------------------------------

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = (NodeBT(f n) (mapBT f lt)  (mapBT f rt))

-------------------------------------

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n EmptyBT EmptyBT) = if a < n then NodeBT n (insert a EmptyBT) EmptyBT
                                             else NodeBT n EmptyBT (insert a EmptyBT)
insert a (NodeBT n lt rt) = if a < n then NodeBT a (insert a lt) rt
                                     else NodeBT a lt (insert a rt)

-------------------------------------

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST [x] = NodeBT x EmptyBT EmptyBT
list2BST (x:xs) = insert x (list2BST xs)



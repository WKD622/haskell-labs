--data MyInt = MkMyInt Int
newtype MyInt = MkMyInt Int


instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where 
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromInteger int)

instance Show MyInt where 
    show (MkMyInt i) = "MkMyInt " ++ show i

data BinTree a = Empty | Node a (BinTree a) (BinTree a)

instance Eq a => Eq (BinTree a) where
    Empty == Empty = True
    Empty == _ = False 
    _ == Empty = False
    Node x xl xr == Node y yl yr = x == y && xl == yl && xr == yr
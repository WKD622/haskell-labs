not' :: Bool -> Bool 
not' True = False 
not' False = True 

isItTheAnswer :: String -> Bool 
isItTheAnswer "Love" = True
isItTheAnswer _      = False 

or' :: (Bool,Bool) -> Bool
or' (True, False) = True
or' (False, True) = True
or' (True, True) = True
or' (False, False) = False

and' :: (Bool,Bool) -> Bool
and' (True, False) = False
and' (True, True) = True
and' (False, False) = False 
and' (False, True) = False

nand' :: (Bool,Bool) -> Bool
nand' (True, False) = True
nand' (True, True) = False
nand' (False, False) = True 
nand' (False, True) = True

xor' :: (Bool, Bool) -> Bool
xor' (True,True) = True 
xor' (False, False) = True
xor' (False, True) = False 
xor' (True, False) = False 

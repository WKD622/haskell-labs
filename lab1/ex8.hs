not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt n = 
    case (n>=0) of 
        True -> n 
        _    -> -n

or' :: (Bool, Bool) -> Bool
or' (a,b) = case a of
        True  -> True 
        False -> case b of 
            False -> False
            _     -> True 

and' :: (Bool, Bool) -> Bool
and' (a,b) = case a of
        True -> case b of 
            True -> True 
            False -> False 
        False -> False

nand' :: (Bool, Bool) -> Bool
nand' (a,b) = case a of 
        True -> case b of 
            True -> False 
            False -> True
        False -> True

xor' :: (Bool, Bool) -> Bool 
xor' (a,b) = case a of
        True -> case b of 
            True -> True
            False -> False 
        False -> case b of 
            False -> True 
            True -> False
isPalindrome :: [Char] -> Bool
isPalindrome x = 
    if reverse x == x then True 
    else False

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
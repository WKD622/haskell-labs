sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where
      intFunc x = x^5

 
fac :: Int -> Int
fac n
      | n <  0    = error "n musi być >= 0"
      | n == 0    = 1
      | otherwise = n * fac (n - 1)
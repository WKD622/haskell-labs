sqr :: Double -> Double 
sqr x = x + x

vec2DLen :: (Double, Double) -> Double 
vec2DLen (x,y)=sqrt(x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z)=sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a,b)=(b,a)

threeEquals :: (Double,Double,Double) -> Bool
threeEquals (a,b,c) = if a==b && b==c
    then True
    else False

absInt :: Int -> Int
absInt a = if a < 0
    then a*(-1)
    else a

min2Int :: (Int,Int) -> Int
min2Int (a,b) = if a < b 
    then a
    else b

min3Int :: (Int,Int,Int) -> Int 
min3Int (a,b,c) = 
    if a < b && a < c then a
    else if b < a && b < c then b 
    else c 

min3Int2 :: (Int, Int, Int ) -> Int
min3Int2 (a,b,c) =
    if min2Int(a,b) == a && min2Int(a,c) == a then a 
    else if min2Int(b,a) == b && min2Int(b,c) == b then b
    else c
data CartInt2DVec = MkCarInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCarInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int 
yCoord (MkCarInt2DVec _ y) = y

-------------------------------------

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a 
yCoord' (MkCart2DVec' _ y) = y

-------------------------------------

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

--xCoord'' :: Cart2DVec'' a -> a
--xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

--yCoord'' :: Cart2DVec'' a -> a
--yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal

-------------------------------------

-- sum type exampe (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show 

head' :: List a -> a 
head' EmptyL        = error "head':  the empty list has no head!"
head' (Cons x xs) = x 

-------------------------------------

data ThreeColors = Blue |
                   White |
                   Red 

type ActorName = String 

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red = "Irena Jacob"

-------------------------------------

data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a 
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a 
yCoord3D (Cart3DVec _ y _ ) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

-------------------------------------

data Cart3DVec' a = Cart3DVec' {x' :: a, y' :: a, z' :: a}

-------------------------------------

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle a) = pi * (a^2)
area (Rectangle a b) = a * b

-------------------------------------

data TrafficLight = Green | Orange | Redd

actionFor :: TrafficLight -> String 
actionFor Green = "go!"
actionFor Orange = "be prepared!"
actionFor Redd = "stop!"
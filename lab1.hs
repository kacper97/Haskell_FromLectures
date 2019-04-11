mysubtract :: (Int,Int) -> Int
mysubtract (x,y) = x-y

bools = [True,False] :: [Bool]
nums =[[1],[2]] :: [[Int]]
add' :: Int -> Int -> Int -> Int
add' x y z = x + y + z

copy' ::  a -> (a,a)
copy' a = (a,a)

apply' :: (a -> b) -> a -> b
apply' a = a 

xs = [1,2,3,4,5]
second xs = head (tail xs)

swap' :: (x,y) -> (y,x)
swap' (x,y) = (y,x)

pair' :: x->y -> (x,y)
pair' x y = (x,y)

double' :: num a -> a -> a
double' x = x*2

palindrome :: [a] -> Bool
palindrome :: Char a -> [a] -> Bool
palindrome xs = reverse xs == xs

--f :: a -> a
--twice f :: a -> a
twice :: f -> f (f,x)
twice f x = f (f x)


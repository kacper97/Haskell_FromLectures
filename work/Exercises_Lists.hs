import Data.Char 
--Exercise 1
squares:: [(Int,Int)]
squares = [(x,x^2) | x <- [1..10]]
--Exercise 2
myConstFunc:: [(Int,Int)]
myConstFunc= [(x,1) | x <- [1..5]]
--Exercise 3
tf1 :: [(Int,Int)]
tf1=[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
f1 :: [(Int,Int)]
f1 = [ ( x , y ) | x<-[1..3],y<-[4..5]]
tf2 :: [(Int,Int)]
tf2=[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
f2 :: [(Int,Int)]
f2 =[(x,y)|y<-[4..5],x<-[1..3]]
tf3 :: [(Int,Int)]
tf3=[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
f3 :: [(Int,Int)]
f3 =[(x,y)|x<-[1..3],y<-[4..5]]
--Exercise 4
--Expected , 2 True, 4 True, 7 False
isEven :: Integer -> Bool
isEven n = (n `mod` 2 == 0)
--Exercise 5
doubleAll :: [Integer]->[Integer]
doubleAll xs = [ x*2 | x<-xs]
--Exercise 6
capitalize :: String -> String
capitalize xs = [ toUpper x | x <- xs]
--Exercise 7
sigma:: Integer
sigma = sum[i^2 | i <- [1..100]]
--Exercise 8
sigma':: Integer -> Integer
sigma' n = sum[i^2 | i <- [1..n]]
--Exercise 9
matches :: Integer -> [Integer] -> [Integer]
matches x xs = [x'|x' <- xs, x ==x']
--Exercise 9 Elem
elem' :: Integer -> [Integer] -> Bool
elem' x xs 
  |x `elem` xs =True
  |otherwise=False
--Exercise 10
grid :: Int -> Int -> [(Int,Int)]
grid n n2 = [(x,y) | x<- [0..n], y<-[0..n2]]
-- Exercise 11
square :: Int -> [(Int,Int)]
square n =[(x,y) | x<- grid[n..0], y<-grid [0..n]]
-- Exercise 12
myReplicate :: Int -> a -> [a]
myReplicate x xs= length [x' | x' <-xs, x == x']



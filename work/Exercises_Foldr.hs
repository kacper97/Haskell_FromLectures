--1 
op :: Num a => a -> a -> a
op x y = x*x + y
sumsq :: Integral a => a -> a
sumsq n = foldr op 0 [1..n] 
--2 
lengthr :: [Int] -> Int
lengthr = foldr (\x y -> 1 + y ) 0
--3
minlistr :: [Int] -> Int
minlistr = foldr1 min
--4
myreverse :: [a] -> [a]
myreverse = foldr (\x y -> y ++ [x]) []
--5
myremove :: 

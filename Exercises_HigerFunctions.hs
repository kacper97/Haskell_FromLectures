import Data.Char 
--mymap f xs =[f x | x <- xs]
--mymap f [] = []
--mymap f (x:xs) = f x : mymap f xs
--1
myall :: (a -> Bool) -> [a] -> Bool
myall p [] = True
myall p xs = and [ p x | x <- xs]
--2
myany :: (a -> Bool) -> [a] -> Bool
myany p [] = False
myany p xs = or [ p x | x <- xs]
--3
capitalise :: String -> String 
capitalise xs = map toUpper xs
--4
squareall :: [Int] -> [Int]
--squareall xs = map (\x -> x*x) xs
squareall = map (\x -> x*x) 
--5
addOne :: [Int] -> [Int]
--addOne xs = map (\x -> x+1) xs
addOne = map (\x -> x+1)
--6
nestedreverse :: [String] -> [String]
nestedreverse xs = reverse (map reverse xs)
--7
atfront :: a -> [[a]] -> [[a]]
atfront p xs = map (p: ) xs
--8
lenthsstring :: [String] -> [Int]
lenthsstring xs = map length xs
--9
sumsq :: [Int] -> [Int]
sumsq = map(\x -> (x^2)+x)
--10
filter :: (a -> Bool) -> [a] -> [a] 
filter p = concat.map box where box x = p
filter p xs = [x|x <- xs,p x]
filter p [] = []
filter p (x:xs) 
   | p x = filter p xs
   | otherwise = filter p xs
--11
wvowel :: [Char] -> [Char]
wvowel = concat
--12 
wiv :: [String] -> [String]
wiv 

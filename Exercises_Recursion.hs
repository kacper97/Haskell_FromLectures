sumdown :: Int -> Int
sumdown 0 = 0 
sumdown n = n + sumdown (n-1)

exponention :: Int -> Int -> Int
exponention 0 _ = 0
exponention m 0 = 1
exponention m n = (m * exponention m (n-1))

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci m = (fibonacci (m-1)) +(fibonacci (m-2))

myInit :: [a] -> [a]
myInit [] = []
myInit [_] =[]
myInit (x:xs) = x : myInit xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b:bs)= b && (myAnd bs) 

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss)= xs ++ (myConcat xss)

myReplicate :: Int ->  a -> [a]
myReplicate 0 y = [ ]
myReplicate x y = y : myReplicate (x-1) y

myNth :: [a] -> Int -> a
myNth (x:xs) 0 = x
myNth (x:xs) n = myNth xs (n-1)

myElem :: Eq a => a-> [a] -> Bool
myElem a [] = False
myElem a (x:xs) 
     | a == x    = True 
     | otherwise = a `myElem` xs

mydouble :: Int -> Int
mydouble x = 2*x

mydoubleAll :: [Int] -> [Int]
mydoubleAll xs = [mydouble x | x <-xs]

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
--merge (x:xs) (y:ys) = x : y : merge xs ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take half xs), (drop half xs))
    where
        half = (length xs ) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
--msort xs = merge (msort left) (msort right)
--msort (x:xs) = halve xs let x = xs : merge xs x


mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myTake :: [a] -> Int -> [a]
myTake [] _ = []
myTake xs 0 = xs
myTake (x:xs)n = myTake xs (n-1)

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
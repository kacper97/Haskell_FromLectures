--1
halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take half xs), (drop half xs))
    where
        half = (length xs ) `div` 2

--1 extra
extra :: [a] -> ([a],[a])
extra xs =
    ((take 1 xs), (drop 1 xs))

--2
third :: [a] -> a
third xs = 
     head (tail(tail xs))

--2nd indexing
third' :: [a] -> a
third' xs = xs !! 2

--2nd pattern matching
--third'' :: [a] -> a
--third'' = (x:(y:(:z,z_)))

--2 extra
second :: [a] -> a
second xs = 
     head (tail xs)
--3
safetail :: [x] -> [x]
safetail (x:xs) = if null[xs] then [] else xs

--4
myOr :: Bool -> Bool -> Bool
False `myOr` False = False
_ `myOr` _ = True

--5
lucky :: Integral a => a -> String 
lucky 7  = "Lucky you.... proceed directly to buy a lottery ticket"
lucky 13 = "You, sadly are quite unlucky. Do not, under any circumstances, invest money today"
lucky _  = "Mmmm.... Can't really say...."

--6
first:: (a,b,c) -> a
first(x,_,_) = x
secnd:: (a,b,c) -> b
secnd(_,y,_) = y
trd:: (a,b,c) -> c
trd(_,_,z) = z

--7 
luhnDouble :: Int -> Int
luhnDouble x =
     if (ans >=9) then (ans-9) else ans
     where 
     ans = (x*2)
--7a
luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z c =
     (x'+y+z'+c) `mod` 10 == 0
     where 
     x' = luhnDouble x
     z' = luhnDouble z
--8
luhnGetCheck :: Int -> Int -> Int -> Int
luhnGetCheck x y z = 10 - luhnSum `mod` 10 
      where
      luhnSum = luhnDouble x + y + luhnDouble z
--Exercises 1/5 done on paper
--6
myfunct1=(\x -> x*2)
myfunct2=(\x -> x*2) 4
myfunct3=(\x y -> x*y)3 4
myfunct4=(\x \y -> if x < y then -1
                   | if x == y then 0
				    | 1
				   )3 4
--7
myabs :: Integer -> Integer
myabs x = if (x < 0) then map(\x -> (x*-1))
          | otherwise x
mymax :: Integer -> Integer
mymin :: Integer -> Integer		  


				 
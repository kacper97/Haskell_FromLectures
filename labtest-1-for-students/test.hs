myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

primes :: [Int]
primes = map head (myiterate sieve [2..])
sieve (p:xs) = [x | x <- xs, x `mod` p /= 0 ]

--myprimes = takewhile (< 10000) primes
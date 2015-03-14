-- 31
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime i = not $ any (\ x -> i `mod` x == 0) [2..(i `div` 2 + 1)]

test31 = print $ isPrime 3

-- 32
gcd' x y
    | x < 0 || y < 0 = gcd' (abs x) (abs y)
    | x == 1 || y == 1 = 1
    | x == y = x
    | x > y = gcd' y x
    | otherwise = gcd' (y - x) x

-- 33
coprime x y = 1 == gcd' x y

-- 34
totient n = sum $ map (\ x -> if isPrime x then 1 else 0) [2..n]

-- 35
-- findFactor b v = if 

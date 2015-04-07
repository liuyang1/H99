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
totient n = length $ filter (coprime n) [2..(n+1)]

-- 35
findfactor v (x: xs)
    | v `mod` x == 0 = x
    | otherwise = findfactor v xs
primefactors 1 = []
primefactors v =
    let f0 = findfactor v (filter isPrime [2..(v+1)])
       in f0: primefactors (v `div` f0)

-- 36
encodeI x n [] = ((x, n), [])
encodeI x n (y: xs)
  | x == y = encodeI x (n + 1) xs
  | otherwise = ((x, n), y: xs)
encode [] = []
encode (x: xs) = let (f, s) = encodeI x 1 xs in f: encode s
primefactorsM = encode . primefactors

-- 37
phi mm = product $ map (\(p, m) -> (p - 1) * p ^ (m - 1)) $ primefactorsM mm

-- 38
-- speed test: 34 vs. 37

-- 39
primeR b e = filter isPrime [b..e]

-- 40
-- need e greater than 2
goldbach e = head $ filter (\(a, b) -> isPrime a && isPrime b) $ map (\x -> (x, e - x)) [2..(e `div` 2)]

-- 41
goldbachl b e = map goldbach $ filter even [b..e]
goldbachl' b e thresh = filter (\(x, _) -> x > thresh) $ goldbachl b e

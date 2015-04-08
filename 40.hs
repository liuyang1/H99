import Data.List (minimumBy, delete)
-- 46
-- 47
-- directly user and / or in system, which could take list as param
prodn n = sequence . (replicate n)
condn n = prodn n [True, False]
table2 fn = map fn $ condn 2

-- 48
tablen fn n = map fn $ condn n

-- 49
gray 1 = ["0", "1"]
gray n = map (\x -> "0" ++ x) nx ++ map (\x -> "1" ++ x) (reverse nx)
  where nx = gray (n - 1)

-- TODO
huffOrd x y = if snd x > snd y then GT else LT
huffman xs = let m0 = minimumBy huffOrd xs
                 m1 = minimumBy huffOrd (delete xs m0)
              in (m0, m1)

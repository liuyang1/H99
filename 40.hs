import Data.List (minimumBy, delete)
import Control.Monad (replicateM)
-- 46
-- 47
-- directly user and / or in system, which could take list as param
-- prodn n = sequence . replicate n
prodn = Control.Monad.replicateM
condn n = prodn n [True, False]
table2 fn = map fn $ condn 2

-- 48
tablen fn n = map fn $ condn n

-- 49
gray 1 = ["0", "1"]
gray n = map (\x -> "0" ++ x) nx ++ map (\x -> "1" ++ x) (reverse nx)
  where nx = gray (n - 1)

-- 50 TODO
huffOrd x y = if snd x > snd y then GT else LT
huffman xs = let m0 = minimumBy huffOrd xs
                 m1 = minimumBy huffOrd (delete m0 xs)
              in (m0, m1)

data HTree = Node Float Char String | Branch Float HTree HTree deriving (Show)
-- huffmanT xs [] = 

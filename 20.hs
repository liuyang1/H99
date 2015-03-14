import qualified Data.Map as Map
import Data.List (group, sort)
import Control.Arrow
import Data.Maybe
-- 26
combinations _ [] = []
combinations n al@(x: xs)
    | n >= length al = [al]
    | n == 1 = map (: []) al
    | otherwise = combinations n xs ++ map (\a -> x: a) (combinations (n - 1) xs)

test26 = do print $ combinations 3 "abcdef"

-- 28
predf f x y = (f x) > (f y)
pred' = predf length
lsortg _ [] = []
lsortg predf (x: xs) =
    lsortg predf [y | y <- xs, predf x y]
    ++ [x] ++
    lsortg predf [y | y <- xs, not (predf x y)]
lsort = lsortg pred'

test28 = do print $ lsort ["ab0","d0","fg1","d1","ijkl","m2","o"]

stat s = map (head &&& length) . group . sort $ map length s
pred0 m = predf (\x -> fromJust (lookup (length x) m))
lsort0 x = lsortg (pred0 (stat x)) x

test280 = do print $ stat ["ab0","d0","fg1","d1","ijkl","m2","o"]
test281 = do print $ lsort0 ["ab0","d0","fg1","d1","ijkl","m2","o"]
main = test281

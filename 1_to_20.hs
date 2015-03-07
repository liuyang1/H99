myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs

myButLast :: [a] -> a
myButLast (x: _: []) = x
myButLast (x: xs) = myButLast xs

-- element-at ::
-- not handling exceptional when n >= length(xs)
elementAt xs 0 = head xs
elementAt (_: xs) n = elementAt xs (n - 1)

myLengthIn [] n = n
myLengthIn (_: xs) n = myLengthIn xs (n + 1)
myLength xs = myLengthIn xs 0

myReverse (x: []) = [x]
myReverse (x: xs) = (myReverse xs) ++ [x]

-- 06
isPalindrome ([]) = True
isPalindrome (x: []) = True
isPalindrome (x: xs) = x == (myLast xs) && (isPalindrome $ init xs)

data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten (Elem e) = [e]
flatten (List a) = foldl (++) [] (mapM flatten a)

test07 = do print $ flatten (Elem 5);
            print $ flatten (List [Elem 1, List [Elem 2], Elem 3]);
            -- print []

compress :: [Char] -> [Char]
compress (x: []) = [x]
compress (x: xs) =
    if x == (head xs)
    then compress xs
    else x: (compress xs)

-- 09
heads x [] = []
heads x xs =
    if x == (head xs)
    then x: (heads x (tail xs))
    else []
nots x [] = []
nots x xs =
    if x == (head xs)
    then nots x (tail xs)
    else xs
pack :: [Char] -> [[Char]]
pack ([]) = []
pack (x: xs) = [x: (heads x xs)] ++ (pack (nots x xs))

-- 10
encodeI x n [] = ((n, x), [])
encodeI x n xs =
    if x == (head xs)
    then encodeI x (n + 1) (tail xs)
    else ((n, x), xs)
encode :: [Char] -> [(Integer, Char)]
encode ([]) = []
encode (x: xs) = let (f, s) = (encodeI x 1 xs) in f: (encode s)

-- 11
data EncodeV = Single Char | Multiple Int Char deriving (Show)

encodeM :: [Char] -> [EncodeV]
encodeM ([]) = []
encodeM (x : xs) =
    if x /= (head xs)
    then (Single x): (encodeM xs)
    else let ((f0, f1), s) = (encodeI x 1 xs) in [Multiple f0 f1] ++ (encodeM s)

-- 12
decodeM :: [EncodeV] -> [Char]
decodeM ([]) = []
decodeM (Single c: xs) = c: (decodeM xs)
decodeM (Multiple n c: xs) = (take n (repeat c)) ++ (decodeM xs)

-- 13
-- 14
dupli ([]) = []
dupli (x: xs) = x: x: (dupli xs)

repli [] n = []
repli (x: xs) n = (take n (repeat x)) ++ (repli xs n)

dropEveryI [] n _ = []
dropEveryI (x: xs) n i =
    if i == (n - 1)
    then dropEveryI xs n 0
    else x: dropEveryI xs n (i + 1)
dropEvery xs n = dropEveryI xs n 0

-- 17
split xs 0 = ([], xs)
split (x: xs) n = let (h, lst) = split xs (n - 1) in ((x: h), lst)

-- 18
slice xs 0 n = fst (split xs n)
slice (x: xs) b e = slice xs (b - 1) (e - 1)

-- 19
rotateI xs 0 = xs
rotateI (x: xs) n = rotateI (xs ++ [x]) (n - 1)
rotate xs n = rotateI xs (mod n (length xs))

-- 20
removeAt (x: xs) 0 = (x, xs)
removeAt (x: xs) n = let (d, t) = removeAt xs (n - 1) in (d, x: t)

-- 21
insertAt v xs 0 = v: xs
insertAt v (x: xs) n = x: (insertAt v xs (n - 1))

-- 22
ranger b e =
    if b == e
    then [b]
    else b: ranger (b + 1) e

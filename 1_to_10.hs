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
-- TODO
flatten (List (NestedList ns): xss) = (flatten ns) ++ (flatten xss)

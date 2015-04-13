-- 54A skip
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- 61
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ t0 t1) = countLeaves t0 + countLeaves t1

-- 61A
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ t0 t1) = leaves t0 ++ leaves t1

-- 62
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a t0 t1) = [a] ++ internals t0 ++ internals t1

-- 62B
-- warning: not handle exception greater level than height
-- level start with 1
atLevel (Branch a t0 t1) 1 = [a]
atLevel (Branch _ t0 t1) n = atLevel t0 n1 ++ atLevel t1 n1
  where n1 = n - 1

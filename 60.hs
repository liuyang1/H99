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

-- 63
maxNodes h = 2 ^ h - 1
minHeight n = ceiling $ logBase 2 (n + 1)

completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch "x" Empty Empty
completeBinaryTree n = let h = (minHeight n) - 1
                           ml = maxNodes h
                           mr = maxNodes (h - 1)
                           thr = ml + mr
                           dn = n - 1
                           nl = if dn > thr then ml else dn - mr
                           nr = if dn > thr then dn - ml else mr
                        in Branch "x" (completeBinaryTree nl) (completeBinaryTree nr)

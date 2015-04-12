-- 54A skip
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
-- 55
cbalTree n
  | n == 0 = [Empty]
  | odd n = let ts = cbalTree ((n - 1) `div` 2)
             in [Branch "x" t0 t1 | t0 <- ts, t1 <- ts]
  | even n = let ts0 = cbalTree (n `div` 2)
                 ts1 = cbalTree (n `div` 2 - 1)
              in [Branch "x" t0 t1 | t0 <- ts0, t1 <-ts1]
              ++ [Branch "x" t1 t0 | t0 <- ts0, t1 <-ts1]

-- 56
-- symmetric
mirror Empty = Empty
mirror (Branch a t0 t1) = Branch a (mirror t1) (mirror t0)
samest Empty Empty = True
samest (Branch a a0 a1) (Branch b b0 b1) = samest a0 b0 && samest a1 b1
samest t0 t1 = False
symmetric t = samest t $ mirror t

-- 57
construct [] = Empty
construct (x0: xs) = let t0 = construct [x | x <- xs, x <= x0]
                         t1 = construct [x | x <- xs, x > x0]
                      in Branch x0 t0 t1

-- 58
symCbalTrees n = [x | x <- cbalTree n, symmetric x]

-- 59
hbalTree c 0 = [Empty]
hbalTree c 1 = [Branch c Empty Empty]
hbalTree c n = let tn1 = hbalTree c (n - 1)
                   tn2 = hbalTree c (n - 2)
                in [Branch c t1 t2 | t1 <- tn1, t2 <- tn2]
                ++ [Branch c t1 t2 | t1 <- tn2, t2 <- tn1]
                ++ [Branch c t1 t2 | t1 <- tn2, t2 <- tn2]

-- 60
-- TODO

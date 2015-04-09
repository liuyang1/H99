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

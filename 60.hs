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

-- 64
rightest (Branch (_, (v, _)) _ Empty) = v
rightest (Branch (_, (_, _)) _ t) = rightest t
layout t = layouth t 1 1

layouth Empty _ _ = Empty
layouth (Branch c tl tr) ox oy = let nl = layouth tl ox (oy + 1)
                                     nx = if nl /= Empty then 1 + rightest nl else ox
                                     nr = layouth tr (nx + 1) (oy + 1)
                                  in Branch (c, (nx , oy)) nl nr

-- 65
width Empty = 0
width (Branch _ Empty Empty) = 1
width (Branch _ tl tr) = 1 + 2 * max (width tl) (width tr)
layout65 Empty _ _ = Empty
layout65 (Branch c tl tr) ox oy = let w = max (width tl) (width tr)
                                      nl = layout65 tl ox (oy + 1)
                                      nr = layout65 tr (ox + w + 1) (oy + 1)
                                   in Branch (c, (ox + w, oy)) nl nr

-- 66
layout66 Empty _ _ = Empty
-- layout66 (Branch c tl tr) ox oy = let w 

-- 67
indexFirsth c [] pos = -1
indexFirsth c (h:t) pos = if c == h then pos else indexFirsth c t (pos + 1)
indexFirst c s = indexFirsth c s 0
indexLasth c [] pos mc = mc
indexLasth c (h:t) pos mc = let nmc = if h /= c then mc else pos
                             in indexLasth c t (pos + 1) nmc
indexLast c s = indexLasth c s 0 (-1)
substr s h d = take (d - h) $ drop h s
indexInfixh [] cnt pos = -1
indexInfixh (h:t) cnt pos = let nc = if h == '(' then cnt + 1 else if h == ')' then cnt - 1 else cnt
                             in if h == ',' && cnt == 1 then pos else indexInfixh t nc (pos + 1)
indexInfix s = indexInfixh s 0 0

stringToTree "" = Empty
stringToTree x = let p0 = indexFirst '(' x
                     p1 = indexInfix x
                     p2 = indexLast ')' x
                     node = if p0 == -1 then x else substr x 0 p0
                     left = substr x (p0 + 1) p1
                     right = substr x (p1 + 1) ((length x) - 1)
                  in if node == ""
                        then Empty
                        else Branch node (stringToTree left) (stringToTree right)

-- 69
tree2ds Empty = "."
tree2ds (Branch a tl tr) = a: tree2ds tl ++ tree2ds tr

-- ds2tree need more work
ds2tree "." = Empty

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
-- 70B isTree

-- 70C
nnodes (Node _ []) = 1
nnodes (Node _ xs) = 1 + sum (map nnodes xs)

-- 70
treeToString (Node c xs) = c : concatMap treeToString xs ++ "^"

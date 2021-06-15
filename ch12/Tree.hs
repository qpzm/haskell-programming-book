data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
               Nothing -> Leaf
               Just (l, ret, r) -> Node (unfold f l) ret (unfold f r)

-- Use unfold to write treeBuild
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x + 1, x, x + 1)) 0

main = do
    print $ treeBuild 0 == Leaf
    print $ treeBuild 1 == Node Leaf 0 Leaf
    print $ treeBuild 2 == Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
    print $ treeBuild 3 ==
        Node (Node (Node Leaf 2 Leaf)
               1
               (Node Leaf 2 Leaf))
         0
         (Node (Node Leaf 2 Leaf)
               1
               (Node Leaf 2 Leaf))


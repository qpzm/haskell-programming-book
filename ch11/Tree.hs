data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- insert
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node l y r)
  | x == y = Node l y r
  | x < y  = Node (insert' x l) y r
  | x > y  = Node l y (insert' x r)

-- map
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

-- Traversal
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l x r) = x : preorder l ++ preorder r

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l x r) = inorder l ++ x : inorder r

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l x r) = postorder l ++ postorder r ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."
testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

-- foldTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f x Leaf = x
-- any traversal order
foldTree f x (Node l y r) = foldTree f (f y $ foldTree f x l) r

main = do
    let t1 = insert' 0 Leaf
    print t1
    let t2 = insert' 3 t1
    print t2
    let t3 = insert' 5 t2
    print t3
    mapOkay
    testPreorder
    testInorder
    testPostorder
    print $ foldTree (+) 0 testTree == 6

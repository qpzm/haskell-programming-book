myIterate :: (a -> a) -> a -> [a]
myIterate f base  = base : myIterate f (f base)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f base =
    case f base of
      Nothing -> []
      Just (res, next) -> res : myUnfoldr f next

-- Rewrite myIterate into betterIterate using myUnfoldr
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

main = do
    print $ (==) [0,1,2,3,4,5,6,7,8,9] (take 10 $ myIterate (+1) 0)
    print $ (==) [0,1,2,3,4,5,6,7,8,9] (take 10 $ myUnfoldr (\b -> Just (b, b+1)) 0)
    print $ (==) [0,1,2,3,4,5,6,7,8,9] (take 10 $ betterIterate (+1) 0)

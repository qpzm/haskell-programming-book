import Data.Bifunctor

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' ((Left x):xs) = x : lefts' xs
lefts' ((Right _):xs) = lefts' xs

rights' :: [Either a b] -> [b]
rights' [] = []
rights' ((Left _):xs) = rights' xs
rights' ((Right x):xs) = x : rights' xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])

-- Better solution: Data.Bifunctor.first (x :) l
partitionEithers' ((Left x):xs)  = (x: fst l, snd l)
    where l = partitionEithers' xs
partitionEithers' ((Right x):xs) = Data.Bifunctor.second (x :) l
    where l = partitionEithers' xs

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- General catamorphism for Either values
either' :: (a-> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

-- Use either'
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

main = do
    let l = [Left 1, Right 2, Left 3, Right 4]
    print $ lefts' l == [1, 3]
    print $ rights' l == [2, 4]
    print $ partitionEithers' l == ([1, 3], [2, 4])
    print $ 2 == either' (+1) (\x -> x - 1) (Left (1 :: Integer))
    print $ 0 == either' (+1) (\x -> x - 1) (Right (1 :: Integer))
    print $ Nothing == eitherMaybe' (+1) (Left (1 :: Integer))
    print $ Just 1 == eitherMaybe' (+1) (Right (0 :: Integer))
    print $ Nothing == eitherMaybe'' (+1) (Left (1 :: Integer))
    print $ Just 1 == eitherMaybe'' (+1) (Right (0 :: Integer))

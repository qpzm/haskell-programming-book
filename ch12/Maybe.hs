import Data.Semigroup

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d f Nothing = d
mayybee d f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

-- Try Again!
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x: catMaybes xs

-- Reference
--https://github.com/evturn/haskellbook/blob/master/12/12.05-small-library-for-maybe.hs
-- 6. Write an function that flips `[Maybe a]` to a `Maybe [a]` where a `Nothing`
-- found in the list returns the sole value of `Nothing`.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe ((Just x):xs) = myAppend (Just [x]) (flipMaybe xs)

myAppend :: Semigroup a => Maybe a -> Maybe a -> Maybe a
myAppend Nothing _ = Nothing
myAppend _ Nothing = Nothing
myAppend (Just x) (Just y) = Just (x <> y)

main = do
    print $ 0 == mayybee 0 (+1) Nothing
    print $ 2 == mayybee 0 (+1) (Just 1)
    print $ 0 == fromMaybe 0 Nothing
    print $ 2 == fromMaybe 0 (Just 2)
    print $ listToMaybe [1, 2, 3] == Just 1
    print $ listToMaybe ([] :: [Integer]) == Nothing
    print $ maybeToList (Just 1) == [1]
    print $ maybeToList Nothing == ([] :: [Integer])
    print $ ([] :: [Integer]) == (catMaybes $ take 3 $ repeat Nothing)
    print $ catMaybes [Just 1, Nothing, Just 2] == [1, 2]
    print $ flipMaybe [Just 1, Just 2, Just 3] == Just [1, 2, 3]
    print $ flipMaybe [Just 1, Nothing, Just 3] == Nothing

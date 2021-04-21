import Data.List

f :: (Fractional a, Num a) => a
f = 1.0

g :: (Fractional a) => a
g = 1.0

h :: RealFrac a => a
h = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
--sigmund :: Int -> Int
--sigmund x = myX

myY = 1::Int
--sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
-- sigmund' x = myY

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

--signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a Error!
signifier xs = head (mySort xs)

main = do
    print g
    print h

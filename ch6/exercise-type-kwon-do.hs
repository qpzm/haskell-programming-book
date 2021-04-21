chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = sum [f y|_ <- [1..x]]

main = do
    print $ chk (+ 1) 1 2
    print $ chk (2^) 2 4
    print $ arith (+ 1) 3 4

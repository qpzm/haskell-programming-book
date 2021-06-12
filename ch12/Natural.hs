data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n
  | n == 0 = Just Zero
  | n > 0 = Just $ integerToNat' n
  | otherwise = Nothing

integerToNat' :: Integer -> Nat
integerToNat' 0 = Zero
integerToNat' x = if x > 0 then Succ (integerToNat' (x - 1)) else undefined

main = do
    print $ (==) 0 $ natToInteger Zero
    print $ (==) 1 $ natToInteger $ Succ Zero
    print $ (==) 2 $ natToInteger $ Succ $ Succ Zero
    print $ (==) Nothing $ integerToNat (-1)
    print $ Just Zero == integerToNat 0
    print $ (Just . Succ . Succ $ Zero) == integerToNat 2

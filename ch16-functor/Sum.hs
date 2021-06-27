data Sum a b =
    First a
  | Second b
      deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

main = do
    print $ fmap (+1) (First 1)
    print (Second 2 :: (Sum Integer Integer))

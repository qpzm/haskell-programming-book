import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

--instance Num a => Monoid (Sum a) where
    --mempty = Sum 0
    --mappend (Sum x) (Sum y) = Sum (x + y)

instance Monoid a => Monoid (Optional a) where
   mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    (<>) x Nada = x
    (<>) Nada x = x
    (<>) (Only x) (Only y) = Only (x <> y)

main :: IO()
main = do
    print $ Only (Sum 1) <> Only (Sum 1)
    print $ Only (Product 2) <> Only (Product 4)
    print $ Only (Sum 1) `mappend` Nada
    print $ Only [1] `mappend` Nada

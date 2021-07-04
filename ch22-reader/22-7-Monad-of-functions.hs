{-# LANGUAGE InstanceSigs #-}

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty  r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- Exercise. Implement Reader Monand
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    --fmap f (Reader ra) = Reader $ \r -> f (ra r)
    -- Refactor
    fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
    -- 답 참고
    pure :: a -> Reader r a
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader rf <*> Reader ra = Reader $ \r -> rf r (ra r)

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r

-- Example. getDogRM
newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person =
    Person {
      humanName :: HumanName
      , dogName :: DogName
      , address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
      dogsName :: DogName
      , dogsAddress :: Address
    } deriving (Eq, Show)

person = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    dogAddress <- address
    return $ Dog name dogAddress

getDogRM' :: Reader Person Dog
getDogRM' = Reader getDogRM

main = do
    let l = [1,2,3]
    print $ foo l    -- [2,3,4]
    print $ bar  0 l -- (0, 3)
    print $ froot l  -- ([2,3,4], 3)
    print $ getDogRM person

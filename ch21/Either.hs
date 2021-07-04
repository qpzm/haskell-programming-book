import Prelude hiding (Either, Left, Right)

data Either a b = Left a | Right b
    deriving (Eq, Ord, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either a) where
    pure = Right

    -- Q. 왜 이건 안 될까?
    --Left e <*> x =  x
    -- A. 내 생각
    -- 에러메세지를 보면 Either a (a1 -> b) <*> Either a (a1) = Either a b 꼴이 되어야 함.
    -- Left <*> Left, Left <*> Right 둘 다 Left 가 나와야 하는데 a1, b의 타입을 모르기 때문에
    -- Either a _ 꼴의 Left e 가 우변에 옴.
    {-
    Couldn't match type ‘a1’ with ‘b’
    ‘a1’ is a rigid type variable bound by
    the type signature for:
      (<*>) :: forall a1 b.
               Either a (a1 -> b) -> Either a a1 -> Either a b
    -}

    Left e <*> _ = Left e
    Right f <*> x = fmap f x

instance Foldable (Either a) where
    -- fodlMap :: Monoid  m => (a -> m) -> t a -> m
    foldMap _ (Left e) = mempty
    foldMap f (Right x) = f x

    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z (Left e) = z
    foldr f z (Right x) = f x z

instance Traversable (Either a) where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t  b)
    traverse f (Left e) = pure $ Left e
    traverse f (Right r) = fmap Right (f r)

    -- sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

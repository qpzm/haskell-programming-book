instance Functor ((,) a) where
    fmap f (x,y) = fmap f (x, f y)

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)

-- 책 보고 함
-- Note! return type is not a tuple
instance Foldable ((,) a) where
    -- foldMap :: Monoid  m => (a -> m) -> t a -> m
    foldMap f (_, x) = f x

    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z (_, x) = f x z

-- 책 보고 함
-- Note! return type is e.g. Maybe (,). Tuple 이 안에 들어가기 위해 fmap 사용.
instance Traversable ((,) a) where
    -- traverse :: Applicative f => (a -> f b) -> t a -> f (t  b)
    traverse f (u, x) = (,) u <$> f x

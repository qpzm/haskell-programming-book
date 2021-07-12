{-# Language InstanceSigs #-}

-- Write State for yourself
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) =
        Moi $ \s ->
            let (h, s') = f s
                (x, s'') = g s'
             in (h x, s'')

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s -> let (a, s') = f s in runMoi (g a) s'
    -- This is equivalent to
    --  let (x, s') = f s
    --      (Moi h) = g x
    --      in h s'

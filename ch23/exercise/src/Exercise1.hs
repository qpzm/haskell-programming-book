{-# Language InstanceSigs #-}

module Ch23.Exercises where

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State sa) = State $ \s -> let (a, s') = sa s in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State sab) <*> (State sa) =
        State $ \s ->
            let (f, s') = sab s
                (a, s'') = sa s' in
                (f a, s'')

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    State sa >>= f = State $ \s ->
        let (a, s') = sa s in
            runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

main = do
    print $ runState get "curryIsAmaze" == ("curryIsAmaze","curryIsAmaze")
    print $ runState (put "blah") "woot" == ((),"blah")
    print $ exec (put "wilma") "daphne" == "wilma"
    print $ exec get "scooby papu" == "scooby papu"
    print $ eval get "bunnicula" == "bunnicula"
    print $ runState (modify (+1)) 0 == ((),1)
    print $ runState (modify (+1) >> modify (+1)) 0 == ((),2)

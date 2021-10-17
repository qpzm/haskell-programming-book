-- 1. []
pure :: a -> [a]
(<*>) :: [a -> b] -> [a] -> [b]

-- 2. IO
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. (, ) a
-- Check in REPL through :i (,) Int
pure :: b -> (a, b)
(<*>) :: (a, b -> c) -> (a, b) -> (a, c)

-- 4. (, ) a
-- Check in REPL through :i -> Int
pure :: a -> (e -> a)
(<*>) :: (e -> a -> b) -> (e -> a) -> (e -> b)

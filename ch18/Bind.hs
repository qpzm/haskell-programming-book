-- Wrtie bind using fmap, join
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f l = join (fmap f l)

main = do
    bind putStrLn getLine

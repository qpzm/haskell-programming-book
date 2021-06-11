module Division where

import Data.Bifunctor

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
    if num < denom
       then (0, num)
       else Data.Bifunctor.first (+1) $ dividedBy (num - denom) denom

multiply :: (Eq a, Num a) => a -> a -> a
multiply x y = if y == 0 then 0 else x + multiply x (y - 1)

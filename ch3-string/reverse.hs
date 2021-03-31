-- How to run
-- ghc -main-is Reverse 5-rvrs.hs -o out;./out

module Reverse where

import Data.List

dropOne = drop 1

yell x = x ++ "!"

-- NOTE index starts from 0
takeFifth x = (!!) x 4

thirdWord :: String -> String
thirdWord x = words x !! 2

letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome"

rvrs :: String -> String
rvrs str = concat [z, " ", y, " ", x]
    where x = take 5 str
          y = take 2 $ drop 6 str
          z = drop 9 str

main :: IO()
main = do
    print $ dropOne "Hello World"
    print $ yell x
    print $ takeFifth x
    print $ thirdWord x
    print $ letterIndex 3
    print $ rvrs x
        where x = "Curry is awesome"

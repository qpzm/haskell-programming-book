module Cipher where

import Data.Char

numToAlphabet = chr . (+ 97) . (`mod` 26) . flip (-) 97

-- offset = 3
caesar :: String -> String
caesar = map $ numToAlphabet . (+ 3) . ord

unCaesar = map $ numToAlphabet . flip (-) 3 .ord

main = do
    print $ caesar "hi"
    print $ caesar "kl"

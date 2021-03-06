module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , letterToMorse
    , morseToLetter
    ) where

import qualified Data.Map as M

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList
    [
      ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
   ]

-- Note!
-- M.foldrWithKey :: (k -> a -> b -> b) -> b -> M.Map k a -> b
-- M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
--
-- flip changes the first two parameters and return a function
-- flip M.insert :: Ord k => a -> k -> (M.Map k a -> M.Map k a)
morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

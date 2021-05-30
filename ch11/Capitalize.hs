import Data.Char (toUpper, isSpace)
import Data.List (dropWhileEnd)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (head : tail) = toUpper head : tail

capitalizeParagraph :: String -> String
capitalizeParagraph str = trimRight . foldr ((\x y -> x ++ ". " ++ y) . capitalizeWord . trimLeft) "" $ split '.' str

-- Reference : https://stackoverflow.com/a/24792141/9379031
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- Reference : https://rosettacode.org/wiki/Strip_whitespace_from_a_string/Top_and_tail#Haskell
trimRight :: String -> String
trimRight = dropWhileEnd isSpace

trimLeft = dropWhile isSpace
trim = trimRight. trimLeft

-- trimLeft is better
--lstrip :: String -> String
--lstrip [] = []
--lstrip (' ' : tail) = lstrip tail
--lstrip (head : tail) = head : tail

main = do
    let s = "blah. woot ha."
    print $ capitalizeParagraph s  == "Blah. Woot ha."

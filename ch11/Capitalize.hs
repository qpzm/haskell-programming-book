import Data.Char (toUpper, isSpace)
import Data.List (dropWhileEnd)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (head : tail) = toUpper head : tail

-- Delete trailing whitespace through trimRight -> 이걸 어떻게 뺄 수 있을까?
capitalizeParagraph :: String -> String
-- Solution 1.
--capitalizeParagraph str = trimRight . foldr ((\x y -> x ++ ". " ++ y) . capitalizeWord . trimLeft) "" $ split '.' str

-- Solution 2.
capitalizeParagraph = trimRight . join ". " . map (capitalizeWord . trimLeft) . split '.'

join :: Monoid a => a -> [a] -> a
join d = foldr (\x y -> x <> d <> y) mempty

-- 위처럼 추상화하기 전
--join :: String -> [String] -> String
--join d [] = []
--join d (x:xs) = x ++ foldr(\x y -> x ++ d ++ y) "" xs

-- Reference : https://stackoverflow.com/a/24792141/9379031
-- Calculation Step
-- split "blah. woot ha"
-- "blah" : drop 1 ". woot ha"
-- "blah" : " woot ha"
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
    let s = "blah. woot ha. hihi."
    print $ capitalizeParagraph s == "Blah. Woot ha. Hihi."

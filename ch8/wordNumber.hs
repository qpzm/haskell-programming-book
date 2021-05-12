import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Input must be one-digit"

digits :: Int -> [Int]
digits n =
    case q of
      0 -> [r]
      _ -> digits q ++ [r]
    where (q, r) = divMod n 10

wordNumber :: Int -> String
wordNumber n = myIntersperse $ map digitToWord (digits n)

myIntersperse l =
    foldr (\x y -> if (y == "") then x ++ y else x ++ "-" ++ y) "" l

main = do
    print $ wordNumber 1
    print $ wordNumber 123
    print $ wordNumber 123456
    print $ wordNumber 100

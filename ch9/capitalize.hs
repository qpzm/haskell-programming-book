import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

capitalizeAll "" = ""
capitalizeAll (x : xs) = toUpper x : capitalizeAll xs

-- You can make it point-free
--capitalizeHead xs = toUpper $ head xs
capitalizeHead = toUpper . head

main = do
    print $ capitalize ""
    print $ capitalize "julie"
    print $ capitalizeAll ""
    print $ capitalizeAll "julie"
    print $ capitalizeHead "julie"

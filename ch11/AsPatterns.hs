import Data.Char

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- Find if target is a subsequence of the input
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf target@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf target ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map makeCapitalizedPair . split ' '

makeCapitalizedPair [] = ([], [])
makeCapitalizedPair w@(x:xs) = (w, toUpper x : xs)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split d s = let (x, y) = span (/= d) s in x : split d (drop 1 y)

main = do
    f (1, 2)
    print $ doubleUp [1, 2] == [1, 1, 2]
    print $ capitalizeWords "hello world"
    print $ isSubseqOf "blah" "blahwoot"
    print $ isSubseqOf "blah" "wootblah"
    print $ isSubseqOf "blah" "wboloath"
    print $ not (isSubseqOf "blah" "wootbla")
    print $ not (isSubseqOf "blah" "halbwoot")
    print $ isSubseqOf "blah" "blawhoot"

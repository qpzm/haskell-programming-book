import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- makes 3-tuples of all possible stop-vowel-stop combinations.
main = do
    print $ combos stops vowels stops

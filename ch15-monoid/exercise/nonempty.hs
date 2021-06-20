import Data.List.NonEmpty

--data NonEmpty a = a :| [a]
    --deriving (Eq, Ord, Show)

--newtype NonEmpty a =
    --NonEmpty (a, [a])
    --deriving (Eq, Ord, Show)

xs = 1 :| [2, 3]
ys = 4 :| [5, 6]

main = do
    print $ xs <> ys

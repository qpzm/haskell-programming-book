validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
       then Nothing
       else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person =
    Person Name Address
    deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson s a = Person <$> mkName s <*> mkAddress a

main = do
    print $ mkPerson (concat $ replicate 26 "a") "Seoul" -- Nothing
    print $ mkPerson (concat $ replicate 25 "a") "Seoul" -- Just (Person (Name "aaaaaaaaaaaaaaaaaaaaaaaaa") (Address "Seoul"))

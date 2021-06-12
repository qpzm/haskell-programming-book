module Person where

data Either a b = Left a | Right b deriving (Show)

type Name = String
type Age = Integer
type ValidatePerson a = Person.Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow
    deriving (Eq, Show)

mkPerson'' :: Name -> Age -> Person.Either PersonInvalid Person
mkPerson'' name age
  | name /= "" && age >= 0 =
    Person.Right $ Person name age
  | name == "" = Person.Left NameEmpty
  | otherwise = Person.Left AgeTooLow

-- Let's put errors as list

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
                True -> Person.Right age
                False -> Person.Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
                  True -> Person.Right name
                  False -> Person.Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Person.Right name) (Person.Right age) =
  Person.Right (Person name age)

mkPerson' (Person.Left [NameEmpty]) (Person.Right _) =
  Person.Left [NameEmpty]

mkPerson' (Person.Right _) (Person.Left [AgeTooLow]) =
  Person.Left [AgeTooLow]

mkPerson' (Person.Left [NameEmpty]) (Person.Left [AgeTooLow]) =
  Person.Left [NameEmpty, AgeTooLow]

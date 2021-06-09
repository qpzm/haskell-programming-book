import Data.List
import Data.Char

-- Ex1.
-- Phone data type 설계는 아래 소스 참고
-- Reference : https://github.com/andrewMacmurray/haskell-book-solutions/blob/master/src/ch11/phone.hs
data Button = Button Digit String deriving Show
data Phone = Phone [Button] deriving Show
phone :: Phone
phone = Phone
  [ Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '*' ""
  , Button '0' " 0"
  , Button '#' "."
  ]

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol", "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol", "Lol ya",
     "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and
type Presses = Int

-- Ex2.
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone c = processCapital [] c ++ findButton phone (toLower c)

findButton :: Phone -> Char -> [(Digit, Presses)]
findButton (Phone buttons) c = [calculatePresses c $ head $ filter (containsCharacter c) buttons]

containsCharacter :: Char -> Button -> Bool
containsCharacter c (Button _ xs) = c `elem` xs

calculatePresses :: Char -> Button -> (Digit, Presses)
calculatePresses c (Button d l) =
    case l of
      [] -> error ""
      x:xs -> if x == c then (d, 1) else (d, 1 + snd (calculatePresses c (Button d xs)))


capitals = ['A' .. 'Z']

processCapital :: [(Digit, Presses)] -> Char -> [(Digit, Presses)]
processCapital l c = if c `elem` capitals then ('*', 1) : l else l

--
cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- Ex3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- Ex4.
mostPopularLetter :: String -> Char
mostPopularLetter str = fst $ foldr maxBySnd (' ', 0) $ calcOccurence str

-- Ex5.
coolestLtr :: [String] -> Char
-- Q. Why error?
-- coolestLtr paragraph = mostPopularLetter . concat paragraph
coolestLtr paragraph = mostPopularLetter $ concat paragraph

maxBySnd :: (Char, Int) -> (Char, Int) -> (Char, Int)
maxBySnd x y = if snd x > snd y then x else y

calcOccurence :: String -> [(Char, Int)]
calcOccurence l = [(x, countInString x l)| x <- allChars]

countInString :: Char -> String -> Int
countInString c l = foldr (\_ x -> 1 + x) 0 $ filter (== c) l

allChars = f $ getButtonsFromPhone phone

getButtonsFromPhone (Phone l) = l

f :: [Button] -> String
f = foldr (\x y -> strFromButton x ++ y) ""

strFromButton (Button d str) = str

--occurencesByChar :: String -> [(Char, Int)]
--occurencesByChar xs = [(x, (length . filter (==x)) xs) | x <- alphaNumChars]

coolestWord :: [String] -> String
coolestWord = undefined

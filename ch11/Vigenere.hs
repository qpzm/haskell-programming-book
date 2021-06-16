import Data.Char

-- 전체는 대문자라고 가정
-- How to handle whitespace? -> 일단 ' ' 를 예외처리해서 해결했으나 깔끔하지 않음.

base = ord 'A'

cipher :: String -> String -> String
cipher key str =
    shiftRight str offsets
        where offsets = calcOffsets $ makeKey str key

decipher :: String -> String -> String
decipher key str =
    shiftLeft str offsets
        where offsets = calcOffsets $ makeKey str key

makeKey :: String -> String -> String
makeKey input key = consume input $ cycle key

consume :: String -> String -> String
consume [] key = ""
consume (x:xs) (y:ys) = if x == ' ' then ' ' : consume xs (y:ys) else y : consume xs ys

calcOffsets :: String -> [Int]
calcOffsets = map (\x -> if x == ' ' then 0 else ord x - base)

shiftRight :: String -> [Int] -> String
shiftRight = zipWith (curry shiftRightChar)

shiftLeft :: String -> [Int] -> String
shiftLeft = zipWith (curry shiftLeftChar)

-- 맨 끝으로 가면 'A' 로 rotate
shiftRightChar :: (Char, Int) -> Char
shiftRightChar (c, n) = if c == ' ' then ' ' else chr $ base + (ord c + n - base) `mod` 26

shiftLeftChar :: (Char, Int) -> Char
shiftLeftChar (c, n) = if c == ' ' then ' ' else chr $ base + (ord c - n - base) `mod` 26

main = do
    let plaintext = "MEET AT DAWN"
    let keyword = "ALLY"
    let shifts = "ALLY AL LYAL"
    let ciphertext = "MPPR AE OYWY"
    print $ makeKey plaintext keyword
    print $ shiftRightChar ('Z', 3)
    print $ shiftLeftChar ('A', 3)
    print $ cipher keyword plaintext
    print $ decipher keyword ciphertext

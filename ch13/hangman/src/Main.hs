module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords =
    let gameLength w = length w >= minWordLength && length w < maxWordLength
    in filter gameLength <$> allWords

randomWord :: WordList -> IO String
randomWord wl = do
    let lastIndex = length wl - 1
    randomIndex <- randomRIO (0, lastIndex)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        intersperse ' '
            (fmap renderPuzzleChar discovered)
            ++ " Guessed so far : " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x (replicate (length x) Nothing) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordToGuess _ _) = charInString wordToGuess

charInString :: String -> Char -> Bool
charInString "" _ = False
charInString (x:xs) c = (x == c) || charInString xs c

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = charInString guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x) = x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wordToGuess discovered guessed) c =
    Puzzle wordToGuess newFilledInSoFar (c : guessed)
        where newFilledInSoFar = zipWith zipper wordToGuess discovered
              zipper charInWord filled =
                  case (charInWord == c, filled) of
                    (True, _) -> Just c
                    (False, Just x) -> Just x
                    (False, Nothing) -> Nothing

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True) -> do
          putStrLn "You already guessed that character, pick something else!"
          return puzzle
      (True, False) -> do
          putStrLn "This character was in the word, filling in the word accordingly"
          return (fillInCharacter puzzle guess)
      (False, False) -> do
          putStrLn "This character wasn't in the word, try again"
          return (fillInCharacter puzzle guess)



gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 7 then
                            do putStrLn "You Lose!"
                               putStrLn $ "The word was " ++ wordToGuess
                               exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
                                do putStrLn "You win!"
                                   exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStrLn "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must\
                    \ be a single character"

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

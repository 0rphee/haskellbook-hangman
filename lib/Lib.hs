module Lib where

import Control.Monad (forever)
import Data.Char (toLower) 
import Data.Maybe (isJust) 
import Data.List (intersperse) 
import System.Exit (exitSuccess) 
import System.Random (randomRIO)

type WordlList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordlList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w = let l = length (w :: String)
                         in l >= minWordLength &&
                          l < maxWordLength

allWords :: IO WordlList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

randomWord :: WordlList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0,maxIndex)
    return $ wl !! randomIndex
    where maxIndex = length wl - 1

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = 
    Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' '$
        fmap renderPuzzleChar discovered) 
        ++ " Guessed so far: " ++guessed++
        " ("++show (length guessed)++" guesses)"

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str nothings []
    where nothings = fmap (const Nothing) str

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just char) = char
 
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c:s)
    where zipper :: Char -> Char -> Maybe Char -> Maybe Char
          zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: "++[guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_,True) -> do
            putStrLn "You already guessed that\
                     \ character, pick \
                     \ something else!"
            return puzzle
        (True,_) -> do
            putStrLn "This character was in the\
                     \ word, filling in the word\
                     \ accordingly"
            return (fillInCharacter puzzle guess)
        (False,_) -> do 
            putStrLn "This character wasn't in\
                     \ the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if incorrectGuesses > 9 then
        do putStrLn "You lose!"
           putStrLn $ "The word was "++wordToGuess
           exitSuccess
    else return ()
    where inWord curr = curr `elem` wordToGuess
          incorrectGuesses = length (filter inWord guessed)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _) =
    if all isJust filledInSoFar then
        do putStrLn $ "The word was  s" ++ wordToGuess
           putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do 
    gameOver puzzle
    gameWin puzzle
    putStrLn $
        "Current puzzle is: "++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must\
                        \ be a single character"

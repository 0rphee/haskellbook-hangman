module Main where

import Test.QuickCheck

import Lib
import Text.Read (Lexeme(Char))
-- TODO add quickcheck and hspec, here and in .cabal 

main :: IO ()
main = do
    quickCheck checker

instance Arbitrary Puzzle where
  arbitrary = do
    w <- arbitrary
    let nothings = replicate (length w) Nothing
    return (Puzzle w nothings [])

-- fillInCharacter
-- handleGuess
checker :: Puzzle -> Char -> Bool
checker puzz@(Puzzle word guessedList tries) charEntered =
  if charEntered `elem` word
  then isMaybeListCorrect && isTriedListCorrect
  else isTriedListCorrect

  where triedPuzz@(Puzzle _ finalGuessedList finalTries) =
          fillInCharacter puzz charEntered
        checkTriedList :: [Char] -> [Char] -> Char-> Bool
        checkTriedList init fin curChar = if curChar `elem` init
                                          then init == fin
                                          else curChar `elem` fin
        isTriedListCorrect = checkTriedList tries finalTries charEntered
        zipper :: Char -> Char -> Maybe Char -> Maybe Char
        zipper guessed wordChar guessChar = if wordChar == guessed
                                            then Just wordChar
                                            else guessChar
        newFilledInSoFar = zipWith (zipper charEntered) word guessedList
        isMaybeListCorrect = finalGuessedList == newFilledInSoFar

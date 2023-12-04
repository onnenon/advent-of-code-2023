module DayOne where

import Control.Applicative
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

data NumPosition = First | Last

getLines :: IO [String]
getLines = do
    fileContents <- readFile "input.txt"
    return $ lines fileContents

sumMaybeInts :: [Maybe Int] -> Int
sumMaybeInts = sum . catMaybes

-- | Given a string, return the first and last digit as an Int
getNumberForLine :: String -> Maybe Int
getNumberForLine (x : xs) = do
    firstDigit <- getLineNumOrWord First [x] xs
    lastDigit <- getLineNumOrWord Last [head reversed] (tail reversed)
    readMaybe [firstDigit, lastDigit]
  where
    reversed = reverse (x : xs)
getNumberForLine [] = Nothing

getLineNumOrWord :: NumPosition -> String -> String -> Maybe Char
getLineNumOrWord pos part [] = findNumberString pos part
getLineNumOrWord pos part (x : xs) = findNumberString pos part <|> getLineNumOrWord pos (part ++ [x]) xs

findNumberString :: NumPosition -> String -> Maybe Char
findNumberString pos p
    | isDigit lastPart = Just lastPart
    | "one" `isInfixOf` part = Just '1'
    | "two" `isInfixOf` part = Just '2'
    | "three" `isInfixOf` part = Just '3'
    | "four" `isInfixOf` part = Just '4'
    | "five" `isInfixOf` part = Just '5'
    | "six" `isInfixOf` part = Just '6'
    | "seven" `isInfixOf` part = Just '7'
    | "eight" `isInfixOf` part = Just '8'
    | "nine" `isInfixOf` part = Just '9'
    | otherwise = Nothing
  where
    part = case pos of
        First -> p
        Last -> reverse p
    lastPart = last p

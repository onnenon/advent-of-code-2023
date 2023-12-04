module DayOne where

import Control.Applicative
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

data NumPosition = First | Last

-- | Given a string, return the first and last digit as an Int
getNumberFromLine :: String -> Maybe Int
getNumberFromLine (x : xs) = do
    firstDigit <- getLineNumOrWord First [x] xs
    lastDigit <- getLineNumOrWord Last [revHed] (tail reversed)
    readMaybe [firstDigit, lastDigit]
  where
    reversed = reverse (x : xs)
    revHed = head reversed
getNumberFromLine [] = Nothing

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

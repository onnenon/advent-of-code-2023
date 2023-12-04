module DayOne where

import Control.Applicative
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Text.Read (readMaybe)

-- | Given a string, return the first and last digit as an Int
getNumberFromLine :: String -> Maybe Int
getNumberFromLine (x : xs) = do
    firstDigit <- getFirstNumOrWord [x] xs
    lastDigit <- getLastNumOrWord " " reversed
    readMaybe [firstDigit, lastDigit]
  where
    reversed = reverse (x : xs)
getNumberFromLine [] = Nothing

getFirstNumOrWord :: String -> String -> Maybe Char
getFirstNumOrWord part [] = findNumberString False part
getFirstNumOrWord part (x : xs) = findNumberString False part <|> getFirstNumOrWord (part ++ [x]) xs

getLastNumOrWord :: String -> String -> Maybe Char
getLastNumOrWord part [] = findNumberString True $ reverse part
getLastNumOrWord part (x : xs) = findNumberString True part <|> getLastNumOrWord (part ++ [x]) xs

findNumberString :: Bool -> String -> Maybe Char
findNumberString rev p
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
    part = if rev then reverse p else p
    lastPart = last p

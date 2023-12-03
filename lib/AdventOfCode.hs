module AdventOfCode where

import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe
import Text.Read (readMaybe)

main :: IO ()
main = do
    calibrationDoc <- readFile "input.txt"
    let calibrationLines = lines calibrationDoc
    print $
        sum $
            map
                ( Data.Maybe.fromMaybe 0 . getNumberFromLine
                )
                calibrationLines

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
getFirstNumOrWord part []
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
    lastPart = last part
getFirstNumOrWord part (x : xs)
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
    | otherwise = getFirstNumOrWord (part ++ [x]) $ xs
  where
    lastPart = last part

getLastNumOrWord :: String -> String -> Maybe Char
getLastNumOrWord part []
    | isDigit lastPart = Just lastPart
    | "one" `isInfixOf` revPart = Just '1'
    | "two" `isInfixOf` revPart = Just '2'
    | "three" `isInfixOf` revPart = Just '3'
    | "four" `isInfixOf` revPart = Just '4'
    | "five" `isInfixOf` revPart = Just '5'
    | "six" `isInfixOf` revPart = Just '6'
    | "seven" `isInfixOf` revPart = Just '7'
    | "eight" `isInfixOf` revPart = Just '8'
    | "nine" `isInfixOf` revPart = Just '9'
    | otherwise = Nothing
  where
    lastPart = last part
    revPart = reverse part
getLastNumOrWord part (x : xs)
    | isDigit lastPart = Just lastPart
    | "one" `isInfixOf` revPart = Just '1'
    | "two" `isInfixOf` revPart = Just '2'
    | "three" `isInfixOf` revPart = Just '3'
    | "four" `isInfixOf` revPart = Just '4'
    | "five" `isInfixOf` revPart = Just '5'
    | "six" `isInfixOf` revPart = Just '6'
    | "seven" `isInfixOf` revPart = Just '7'
    | "eight" `isInfixOf` revPart = Just '8'
    | "nine" `isInfixOf` revPart = Just '9'
    | otherwise = getFirstNumOrWord (part ++ [x]) $ xs
  where
    lastPart = last part
    revPart = reverse part

-- [1, n, i, n, e]
-- [e]
-- [e, n]
-- [e, n, i]
-- [e, n, i, n]
-- [e, n, i, n, 1]
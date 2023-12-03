module AdventOfCode (main, getNumberFromLine) where

import Data.Char ( isDigit )
import Data.List ( find )
import Text.Read ( readMaybe )


main :: IO ()
main = do
    calibrationDoc <- readFile "input.txt"
    let calibrationLines = lines calibrationDoc 
    putStrLn $ show $ sum $ map (\x -> case getNumberFromLine x of
        Nothing -> 0
        Just value -> value) calibrationLines


getNumberFromLine :: String -> Maybe Int
getNumberFromLine line = do
    firstDigit <- find isDigit line
    lastDigit <- find isDigit $ reverse line
    readMaybe [firstDigit, lastDigit]

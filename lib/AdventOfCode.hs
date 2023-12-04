module AdventOfCode where

import Data.Maybe (fromMaybe)

import DayOne (getNumberFromLine)

main :: IO ()
main = do
    inputLines <- getLines
    let numbers = map getNumberForLine inputLines
    let calibrationSum = sumMaybeInts numbers
    putStrLn $ show calibrationSum
                    
    -- calibrationDoc <- readFile "input.txt"
    -- let calibrationLines = lines calibrationDoc
    -- print $
    --     sum $
    --         map
    --             ( Data.Maybe.fromMaybe 0 . getNumberFromLine
    --             )
    --             calibrationLines
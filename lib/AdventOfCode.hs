module AdventOfCode where

import DayOne (getLines, getNumberForLine, sumMaybeInts)

main :: IO ()
main = do
    inputLines <- getLines
    let numbers = map getNumberForLine inputLines
    let calibrationSum = sumMaybeInts numbers
    print calibrationSum

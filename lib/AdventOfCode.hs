module AdventOfCode where

import Data.Maybe (fromMaybe)

import DayOne (getNumberFromLine)

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
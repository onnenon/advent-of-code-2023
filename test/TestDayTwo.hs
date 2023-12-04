{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWith)

import DayTwo (isGamePossible)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = describe "isGamePossible" $ for_ cases test
  where
    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion = isGamePossible input `shouldBe` expected

data Case = Case
    { description :: String
    , input :: String
    , expected :: Bool
    }

cases :: [Case]
cases =
    [ Case
        { description = "Correctly IDs Possible Game"
        , input = "1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
        , expected = False
        }
    , Case
        { description = "Correcty IDs Impossible Game"
        , input = "8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        , expected = False
        }
    ]

{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWith)

import DayOne (getNumberForLine)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = describe "getNumberForLine" $ for_ cases test
  where
    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion = getNumberForLine input `shouldBe` expected

data Case = Case
    { description :: String
    , input :: String
    , expected :: Maybe Int
    }

cases :: [Case]
cases =
    [ Case
        { description = "Correctly parses a line with two digits"
        , input = "19qdlpmdrxone7sevennine"
        , expected = Just 19
        }
    , Case
        { description = "Correctly parses a line with two digits"
        , input = "ninefivefive2nine5ntvscdfdsmvqgcbxxxt"
        , expected = Just 95
        }
    , Case
        { description = "Correctly parses a line with two digits"
        , input = "2czddtpsrgsbgddsix6gvmxqlsnnine4"
        , expected = Just 24
        }
    , Case
        { description = "Correctly parses a line with two digits"
        , input = "sixonemc55"
        , expected = Just 65
        }
    , Case
        { description = "Correctly parses a line with two digits"
        , input = "3vpxlzkc"
        , expected = Just 33
        }
    , Case
        { description = "Correctly parses a line with two digits"
        , input = "pxlzkcsevenakeadkekeio"
        , expected = Just 77
        }
    ]

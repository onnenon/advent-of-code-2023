{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWith)

import DayOne (getNumberFromLine)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = describe "getNumberFromLine" $ for_ cases test
  where
    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion = getNumberFromLine input `shouldBe` expected

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
    ]
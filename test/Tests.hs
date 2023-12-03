import Test.HUnit

runTests :: IO Counts
runTests = runTestTT $ TestList [testGetNumberFromLine]

testGetNumberFromLine :: Test
testGetNumberFromLine = TestCase $ assertEqual "for (getNumberFromLine \"123abc456\")," (Just 14) (getNumberFromLine "123abc456")
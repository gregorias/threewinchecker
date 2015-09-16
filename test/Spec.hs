import LibTest
import Test.HUnit
import qualified ScraperLibTest as SLT


allTests :: Test
allTests = test [
    calculatePercentageOfPositiveHypothesisTest
  , SLT.allTests 
  ]

main :: IO ()
main = do
  runTestTT allTests
  return ()

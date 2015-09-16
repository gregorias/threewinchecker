module LibTest (
    calculatePercentageOfPositiveHypothesisTest
  ) where


import Lib
import Test.HUnit


exampleProgress :: [Progress]
exampleProgress = [
    [(25, 0), (25, 0), (25, 0)]
  , [(0, 25), (25, 0), (25, 0), (25, 0)]
  , [(25, 0), (0, 25), (25, 0), (25, 0)]
  , [(25, 0), (25, 0), (0, 25), (25, 0)] -- negative example
  , [(0, 25), (0, 25), (25, 0), (0, 25)] -- negative example
  , [(25, 0), (0, 25), (25, 0), (0, 25), (25, 0)]
  , [(25, 0), (25, 0), (0, 25), (0, 25), (25, 0)] -- negative example
  , [(25, 0), (25, 0), (0, 25), (0, 25), (0, 25)] -- positive example
  ]


exampleMRs :: [MatchResult]
exampleMRs = map (MatchResult "A" "B") exampleProgress


exampleNegatives :: Float
exampleNegatives = 3


examplePositives :: Float
examplePositives = 1


calculatePercentageOfPositiveHypothesisTest :: Test
calculatePercentageOfPositiveHypothesisTest = TestCase
  $ (((examplePositives / (exampleNegatives + examplePositives)))
    @=? calculatePercentageOfPositiveHypothesis exampleMRs)

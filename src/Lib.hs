{-|
Module      : Lib
Description : Base types and analytic tools for threewinchecker
Copyright   : (c) Grzegorz Milka, 2015
License     : GPL-3
Maintainer  : grzegorzmilka@gmail.com
Stability   : stable
Portability : POSIX
-}
module Lib (
    MatchResult(..)
  , Progress
  , Set
  , calculatePercentageOfPositiveHypothesis
  ) where

import Control.Monad
import Data.Csv (
    Field
  , FromRecord(..)
  , Parser
  , ToRecord(..)
  , record
  , toField
  , (.!)
  )

type Team = String

data Side = First | Second deriving (Eq, Show)

type Set = (Int, Int)

type Progress = [Set]


-- | Data describing the result of a volleyball match
data MatchResult = MatchResult {
    mrFirstTeam :: Team
  , mrSecondTeam :: Team
  , mrProgress :: Progress
} deriving (Eq, Show)


instance ToRecord MatchResult where
  toRecord mr = record 
    $ [
        toField $ mrFirstTeam mr
      , toField $ mrSecondTeam mr
      ]
      ++ progressFields (mrProgress mr)
    where
      progressFields :: Progress -> [Field]
      progressFields p = take 10
        . concatMap setFields
        $ p ++ (repeat (0, 0))

      setFields :: Set -> [Field]
      setFields (a, b) = [toField a, toField b]


instance FromRecord MatchResult where
  parseRecord v
    | length v == mrRecordLength = MatchResult 
      <$> v .! 0
      <*> v .! 1
      <*> progressParser
    | otherwise                  = mzero
    where
      mrRecordLength :: Int
      mrRecordLength = 12

      listToProgress :: [Int] -> Progress
      listToProgress (a:b:xs)
        | a /= 0 || b /= 0 = (a, b) : (listToProgress xs)
        | otherwise = []
      listToProgress _ = []

      progressParser :: Parser Progress
      progressParser = fmap listToProgress
        . sequence 
        . map (v .!) 
        $ [2..11]


-- | Returns the side which has won given set.
whoWon :: Set -> Side
whoWon (a, b)
  | a >= b     = First
  | otherwise = Second


-- | Returns true if in given progress a team has lost a set after two wins
areTwoWinsFollowedByALoss :: Progress -> Bool
areTwoWinsFollowedByALoss progress =
  length progress >= 3
    && (sideProgress !! 0 == sideProgress !! 1)
    && (sideProgress !! 1 /= sideProgress !! 2)
  where
    sideProgress = map whoWon progress


-- | Returns true if in given progress a team has lost a match after two wins
areTwoWinsFollowedByThreeLosses :: Progress -> Bool
areTwoWinsFollowedByThreeLosses progress =
  areTwoWinsFollowedByALoss progress
    && length progress >= 5
    && (sideProgress !! 2 == sideProgress !! 3)
    && (sideProgress !! 3 == sideProgress !! 4)
  where
    sideProgress = map whoWon progress


-- | Given match results finds what percentage of matches were lost by a team
-- that has won two initial matches.
calculatePercentageOfPositiveHypothesis :: [MatchResult] -> Float
calculatePercentageOfPositiveHypothesis results =
  let
    anteSatisfied = filter areTwoWinsFollowedByALoss . map mrProgress $ results
    postSatisfied = filter areTwoWinsFollowedByThreeLosses anteSatisfied
    cast = fromRational . toRational
  in
    (cast $ length postSatisfied) / (cast $ length anteSatisfied)

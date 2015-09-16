module Main where

import Data.Csv (
    HasHeader(NoHeader)
  , decode
  )
import Data.Vector (
    toList
  )
import Lib
import System.Environment (
    getArgs
  )

import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn usage
  else do
    let csvFile = head args
    csvContent <- BL.readFile csvFile
    let eitherMrs = toList <$> decode NoHeader csvContent
    case eitherMrs of
      Left msg -> putStrLn $ "Could not decode the csv file: " ++ msg
      Right mrs -> do
        putStrLn $ "Loaded " ++ (show $ length mrs) ++ " records"
        let ratio = calculatePercentageOfPositiveHypothesis mrs
        putStrLn $ "The ratio of instances validating the hypothesis is: " 
          ++ show ratio

usage :: String
usage = "Usage: checker INPUT_FILE"

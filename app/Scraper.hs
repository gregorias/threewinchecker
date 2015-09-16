module Main (main) where

import Control.Exception
import Data.ByteString.Lazy.Char8 (
    unpack
  )
import Data.Csv (
    encode
  )
import Lib
import ScraperLib (
    downloadMatchResultsLog
  )
import System.Environment (
    getArgs
  )
import System.IO (
    IOMode(WriteMode)
  , hClose
  , hPutStr
  , openFile
  )

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn usage
  else do
    let csvFile = head args
    results <- downloadMatchResultsLog putStrLn
    case results of
      Left error -> putStrLn error
      Right mrs -> do 
         let text = unpack . encode $ mrs
         bracket 
           (openFile csvFile WriteMode)
           (\h -> hClose h)
           (\h -> hPutStr h text)

usage :: String
usage = "Usage: scraper OUTPUT_FILE"

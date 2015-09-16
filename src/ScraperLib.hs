{-|
Module      : ScraperLib
Description : Main functions for the scraper tool.
Copyright   : (c) Grzegorz Milka, 2015
License     : GPL-3
Maintainer  : grzegorzmilka@gmail.com
Stability   : stable
Portability : POSIX
-}
module ScraperLib where

import Control.Monad
import Control.Monad.Trans.Class (
    lift
  )
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString (
    Parser
  , parseOnly
  , sepBy
  )
import Data.Attoparsec.ByteString.Char8 (
    char
  , decimal
  , string
  )
import Data.ByteString.Char8 (
    pack
  )
import Data.Char (
    isSpace
  )
import Lib
import Text.HTML.Scalpel (
    Scraper
  , Selector
  , URL
  , (//)
  , (@:)
  , (@=)
  , attr
  , chroots
  , hasClass
  , scrapeURL
  , text
  , texts
  )
import Text.Printf (
    printf
  )

-- | An ExceptT version of Text.HTML.Scalpel.scrapeURL
scrapeURLE :: Scraper String a -> URL -> ExceptT String IO a 
scrapeURLE scraper url = do
  maybeResult <- lift $ scrapeURL url scraper
  case maybeResult of
    Nothing -> throwE $ "Encountered an error during scraping " ++ url
    Just result -> return result

-- | Main scraper function. This function scrapes www.worldofvolley.com for
-- match results.
downloadMatchResults :: IO (Either String [MatchResult])
downloadMatchResults = runExceptT $ do
  competitionPages <- fmap concat
    $ fmap (fmap (take 10 . map (wOVPrefix ++)))
    $ sequence
    $ map (scrapeURLE competitionScraper) seasonPages
  fmap concat . sequence
    $ map (scrapeURLE matchResultScraper) competitionPages

-- | 'downloadMatchResults' with logging
downloadMatchResultsLog :: (String -> IO ()) -> IO (Either String [MatchResult])
downloadMatchResultsLog logFun = runExceptT $ do
  lift $ logFun "Scraping season pages for links to competitions."
  competitionPages <- fmap concat
    $ fmap (fmap (take 10 . map (wOVPrefix ++)))
    $ sequence
    $ map (scrapeURLE competitionScraper) seasonPages
  fmap concat . sequence
    $ map scrapeCompetitionPage competitionPages
  where
    scrapeCompetitionPage :: URL -> ExceptT String IO [MatchResult]
    scrapeCompetitionPage url = do
      lift . logFun $ "Scraping competition page: " ++ url
      scrapeURLE matchResultScraper url

-- | Domain of www.worldofvalley.com
wOVPrefix :: String
wOVPrefix = "http://www.worldofvolley.com"

-- | Printf URL form for season pages
seasonPageForm :: String
seasonPageForm = "http://www.worldofvolley.com/championships/game-schedules.html?kind=&confederationId=0&institutionId=0&countryId=0&ageId=2&season=%d%%2F%d&searchText="

-- | HTTP URLs to pages for volleyball seasons from 2013s
seasonPages :: [String]
seasonPages = [printf seasonPageForm start (start + 1) 
  | start <- [2013 .. 2015] :: [Int]]


-- | Scraper of competition pages
matchResultScraper :: Scraper String [MatchResult]
matchResultScraper = chroots matchResultsRow submatchResultScraper
  where
    submatchResultScraper :: Scraper String MatchResult
    submatchResultScraper = do
      clubs <- texts $ "a" @: [hasClass "clubName"]
      guard $ length clubs == 2
      let [leftClub, rightClub] = map strip clubs
      progressSpan <- text $ "td" @: [hasClass "info"] // "span"
      let parseResult = parseOnly progressParser $ pack progressSpan
      progress <- case parseResult of
        Right r -> return r
        _       -> mzero
      return $ MatchResult leftClub rightClub progress


-- | Selector for table rows with match results
matchResultsRow :: Selector
matchResultsRow = "div" @: ["id" @= "gamesDiv"]
  // "tr"


-- | Scraper which finds links to competition pages from a season page.
competitionScraper :: Scraper String [String]
competitionScraper = 
  chroots competitionSelector
    $ attr "href" "a"
  where
    competitionSelector :: Selector
    competitionSelector = "div" @: [hasClass "competition"]
      // "a" @: [hasClass "button"]

-- | Parser of match progress expression
progressParser :: Parser Progress
progressParser = sepBy setParser 
  ((string $ pack "; ") `mplus` (string $ pack ", "))


-- | Parser of result:result expression
setParser :: Parser Set
setParser = do
  leftResult <- decimal
  _ <- char ':'
  rightResult <- decimal
  return (leftResult, rightResult)


-- | This function strips whitespace from a string.
strip :: String -> String
strip = f . f
  where 
    f = reverse . dropWhile isSpace

module ScraperLibTest (
    allTests
  ) where

import Data.List (
    sort
  )
import Lib
import QQ
import ScraperLib
import Test.HUnit
import Text.HTML.Scalpel (
    scrapeStringLike
  )

exampleSeasonPage :: String
exampleSeasonPage = [qq|<html><body>
  <div class='competition fix'>
    <a href='/championships/competition/3583/european-league-2015.html'> European League 2015 / Female / Seniors</a>
    <a href='/championships/competition/3583/european-league-2015.html' class='button right'>ALL GAMES</a></div>
  <div class='competition fix'>
    <a href='/championships/competition/3547/world-league-2015.html'> World League 2015 / Male / Seniors</a>
    <a href='/championships/competition/3547/world-league-2015.html' class='button right'>ALL GAMES</a></div>
  </body></html>|]

expectedCompetitionLinks :: [String]
expectedCompetitionLinks = [
    "/championships/competition/3583/european-league-2015.html"
  , "/championships/competition/3547/world-league-2015.html"
  ]

exampleCompetitionPage :: String
exampleCompetitionPage = [qq|
  <html><body>
  <div id="gamesStatsDiv" class='tabDiv'>
                    <div class='games' id='gamesDiv'>
                        <table class='schedulesTable'>
                            <thead>
                                <tr>
                                    <td>Game</td>
                                    <td>Date/Time</td>
                                    <td colspan='3'>Teams</td>
                                    <td>Set</td>
                                    <td>Result per set</td>
                                    <td>Stats</td>
                                    <td>Videos</td>
                                </tr>
                            </thead>
                            

                                <tr>
                                    <td class='dateGame'><span>1st game</span></td>
                                    <td class='dateGame'><span>20. 09. 2014  14:00</span></td>
                                            <td><a class='clubName' href='/wov-community/national-teams/16818/chinese-taipei--w--.html'><b>Chinese Taipei</b></a></td>
                                            <td><span class='vs left'>VS</span></td><td><a class='clubName' href='/wov-community/national-teams/18942/hong-kong--w--.html'>Hong Kong</a></td>
                                    <td class='gameSmallResult'><span>3:0</span></td>
                                    <td class='info'><span>25:10; 25:12; 25:19</span></td>
                                    <td class='stats'>
                                    </td>
                                    <td class='videos'>
                                    </td>
                                </tr>

                                <tr>
                                    <td class='dateGame'><span>2nd game</span></td>
                                    <td class='dateGame'><span>21. 09. 2014  16:00</span></td>
                                            <td>
                                            <a class='clubName' href='/wov-community/national-teams/14499/kazakhstan--w--.html'>Kazakhstan</a></td>
                                            <td><span class='vs left'>VS</span></td><td><a class='clubName' href='/wov-community/national-teams/11264/china--w--.html'><b>China</b></a></td>
                                    <td class='gameSmallResult'><span>0:3</span></td>
                                    <td class='info'><span>14:25; 11:25; 14:25</span></td>
                                    <td class='stats'>
                                    </td>
                                    <td class='videos'>
                                    </td>
                                </tr>
      </table></div></div></body></html>|]


expectedMRs :: [MatchResult]
expectedMRs = [
    MatchResult "Chinese Taipei" "Hong Kong" [(25, 10), (25, 12), (25, 19)]
  , MatchResult "Kazakhstan" "China" [(14, 25), (11, 25), (14, 25)]
  ]


competitionPageScrapeTest :: Test
competitionPageScrapeTest = expectedMRs ~=? actual
  where
    actual = 
      case scrapeStringLike exampleCompetitionPage matchResultScraper of
        Nothing -> []
        Just r  -> r

seasonPageScrapeTest :: Test
seasonPageScrapeTest = sort expectedCompetitionLinks ~=? actual
  where 
    actual = 
      case fmap sort 
          $ scrapeStringLike exampleSeasonPage competitionScraper of
        Nothing -> []
        Just r  -> r

allTests :: Test
allTests = test [
    competitionPageScrapeTest
  , seasonPageScrapeTest
  ]

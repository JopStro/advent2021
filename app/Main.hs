module Main where

import DayOne
import DayTwo
import DayThree
import DayFour
import DayFive
import DaySix
import DaySeven
import DayEight
import DayNine
import DayTen
import DayEleven
import DayTwelve
import DayThirteen
import DayFourteen
import DayFifteen
import DaySixteen
import DaySeventeen
import DayEighteen
import DayTwenty
import System.Environment

main :: IO ()
main = getArgs >>= (\(part:args) -> runDay (read part) args)

runDay :: Int -> [String] -> IO ()
runDay 1 (f:_)   = DayOne.floorSteepness f >>= print
runDay 2 (f:_)   = DayTwo.runMovementScript f >>= print
runDay 3 (f:_)   = DayThree.diagnose f >>= print
runDay 4 (f:n:_) = DayFour.bingo f (read n) >>= print
runDay 5 (f:_)   = DayFive.findVents f >>= print
runDay 6 (f:_)   = DaySix.simFile f >>= print
runDay 7 (f:_)   = DaySeven.examineFile f >>= print
runDay 8 (f:_)   = DayEight.analyse f >>= print
runDay 9 (f:_)   = DayNine.analyse f >>= print
runDay 10 (f:_)  = DayTen.validateFile f >>= print
runDay 11 (f:_)  = DayEleven.glow f >>= print
runDay 12 (f:_)  = DayTwelve.pathFinding f >>= print
runDay 13 (f:_)  = DayThirteen.foldPaper f >>= print
runDay 14 (f:_)  = DayFourteen.polymerize f >>= print
runDay 15 (f:_)  = DayFifteen.pathFile f >>= print
runDay 16 (f:_)  = DaySixteen.readPacket f >>= print
runDay 17 (f:_)  = DaySeventeen.trickshot f >>= print
runDay 18 (f:_)  = DayEighteen.sumSnails f >>= print
runDay 20 (f:_)  = DayTwenty.enhanceFile f >>= print
runDay _ []      = print "Invalid arguments: provide input file"
runDay 4 _       = print "Invalid Arguments: provide bingo card height"
runDay _ _       = print "Day not implemented"

module Main where

import DayTwelve
import System.Environment

main :: IO ()
main = getArgs >>= pathFinding . head >>= print

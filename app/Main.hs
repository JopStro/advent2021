module Main where

import DayFive
import System.Environment

main :: IO ()
main = getArgs >>= findVents . head >>= print

module Main where

import DayEight
import System.Environment

main :: IO ()
main = getArgs >>= analyse . head >>= print

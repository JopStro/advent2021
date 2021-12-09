module Main where

import DayNine
import System.Environment

main :: IO ()
main = getArgs >>= analyse . head >>= print

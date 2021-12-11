module Main where

import DayEleven
import System.Environment

main :: IO ()
main = getArgs >>= glow . head >>= print

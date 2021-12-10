module Main where

import DayTen
import System.Environment

main :: IO ()
main = getArgs >>= validateFile . head >>= print

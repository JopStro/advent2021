module Main where

import DayThirteen
import System.Environment

main :: IO ()
main = getArgs >>= foldPaper . head >>= print

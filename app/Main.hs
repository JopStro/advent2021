module Main where

import DaySeven
import System.Environment

main :: IO ()
main = getArgs >>= examineFile . head >>= print

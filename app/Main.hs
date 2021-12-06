module Main where

import DaySix
import System.Environment

main :: IO ()
main = getArgs >>= simFile . head >>= print

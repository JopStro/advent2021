-- |

module DayTwo where

import Data.Char (toUpper)

data Command = Forward  Int
             | Down     Int
             | Up       Int
             deriving (Read)

type ShipData = (Int, Int, Int)

move :: ShipData -> Command -> ShipData
move (x, y, z) (Forward n) = (x+n, y+(n*z), z)
move (x, y, z) (Down    n) = (x, y, z+n)
move (x, y, z) (Up      n) = (x, y, z-n)

readCommand :: String -> Command
readCommand = read . capitalize
  where
    capitalize []     = []
    capitalize (x:xs) = toUpper x : xs

runMovementScript :: String -> IO Int
runMovementScript xs = do
  (x, y, _) <- foldl move (0,0,0) . map readCommand . lines <$> readFile xs
  return $ x * y

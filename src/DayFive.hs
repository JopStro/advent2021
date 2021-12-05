-- |

module DayFive where

import Control.Applicative
import Data.Bool
import Data.List

type Pos = (Int,Int)

type Line = [Pos]

data Alignment = Vert | Hori deriving (Show)

checkPerp :: Line -> Maybe Alignment
checkPerp xs = bool Nothing (Just Hori) (checkDim xlist) <|> bool Nothing (Just Vert) (checkDim ylist)
  where
    checkDim [] = False
    checkDim (y:ys) = not $ any (y/=) ys
    xlist = map fst xs
    ylist = map snd xs

interpolate :: Line -> Line
interpolate xs = case checkPerp xs of
                   Just Hori -> zip (repeat $ head xlist) (safeInter (head ylist) (last ylist))
                   Just Vert -> zip (safeInter (head xlist) (last xlist)) (repeat $ head ylist)
                   Nothing -> zip (safeInter (head xlist) (last xlist)) (safeInter (head ylist) (last ylist))
  where
    xlist = map fst xs
    ylist = map snd xs
    safeInter n m | n > m = reverse [m..n]
                  | m > n = [n..m]
                  | otherwise = [n]

findIntersections :: [Line] -> [Pos]
findIntersections = map head . filter ((>1) . length) . group . sort . concatMap interpolate

parseLines :: [String] -> [Line]
parseLines = map (\xs -> [start xs, end xs])
  where
    start = parsePos . head . words
    end   = parsePos . last . words
    parsePos s = read $ "("++s++")"

findVents :: String -> IO Int
findVents filename = do
  lineList <- parseLines . lines <$> readFile filename
  let intersections = findIntersections lineList
  return $ length intersections

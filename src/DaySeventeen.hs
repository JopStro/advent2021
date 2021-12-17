-- |

module DaySeventeen where
import Data.Bifunctor

type Pos = (Int,Int)
type Vel = (Int,Int)
type Range = (Int,Int)
type Box = (Range,Range)

step :: (Pos,Vel) -> (Pos,Vel)
step ((x,y),(xv,yv)) = ((x+xv,y+yv),(if xv > 0 then xv - 1 else if xv < 0 then xv + 1 else 0, yv - 1))

sim :: Int -> (Pos,Vel) -> [Pos]
sim miny = map fst . takeWhile (\((_, y),_) -> y >= miny) . iterate step

checkSuccessfulY :: Range -> Int -> Bool
checkSuccessfulY (miny,maxy) yh = not . null . takeWhile (<=(yh-miny)) $ dropWhile (<=(yh-maxy)) [((n*n) + n) `div` 2 | n <- [1..]]

findHighest :: Box -> Int
findHighest (_,(miny,maxy)) = maximum $ filter (checkSuccessfulY (miny,maxy)) [((y*y) + y) `div` 2 | y <- [1..100]]

countSuccesses :: Box -> Int
countSuccesses ((minx,maxx),(miny,maxy)) = length $ filter (any (uncurry (&&) . bimap (between minx maxx) (between miny maxy))) [sim miny ((0,0),(x,y)) | y <- [miny..100],x <- [1..maxx]]
  where
    between n m v = (v >= n) && (v <= m)
readRange :: String -> Range
readRange = bimap read (read . dropWhile (=='.')) . break (=='.')

trickshot :: String -> IO (Int,Int)
trickshot xs = do
  box <- bimap readRange (readRange . drop 4) . break (==',') . drop 15 . head . lines <$> readFile xs
  return (findHighest box, countSuccesses box)

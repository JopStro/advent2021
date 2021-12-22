-- |

module DayTwentyTwo where
import Data.Bifunctor

type Range = (Int,Int)
data Rect = Rect {xr::Range,yr::Range,zr::Range}

inRange :: Int -> Range -> Bool
inRange x (s,e) = s <= x && x <= e

inRect :: (Int,Int,Int) -> Rect -> Bool
inRect (x,y,z) rect = inRange x (xr rect) && inRange y (yr rect) && inRange z (zr rect)

notPassed :: Int -> Rect -> Bool
notPassed z (Rect { zr=(_,end) }) = z <= end

sweep :: (Int,Int,Int) -> (Range,Range,Range) -> [(Bool,Rect)] -> Int
sweep (x,y,z) ranges@((minx,maxx),(miny,maxy),(_,maxz)) rects | x > maxx = sweep (minx,y+1,z) ranges rects
                                                              | y > maxy = sweep (x,miny,z+1) ranges rects
                                                              | z > maxz = 0
                                                              | (\bs -> not (null bs) && head bs) . map fst $ filter (inRect (x,y,z) . snd) rects = 1 + sweep (x+1,y,z) ranges rects
                                                              | otherwise = sweep (x+1,y,z) ranges rects
readRect :: String -> Rect
readRect xs = Rect (readRange xrange) (readRange yrange) (readRange zrange)
  where
    (xrange,sxr) = break (==',') $ drop 2 xs
    (yrange,syr) = break (==',') $ drop 3 sxr
    zrange = drop 3 syr

readRange :: String -> Range
readRange = bimap read (read . drop 2) . break (=='.')

readRects :: [String] -> [(Bool,Rect)]
readRects [] = []
readRects (('o':'n':' ':x):xs) = (True,readRect x):readRects xs
readRects (('o':'f':'f':' ':x):xs) = (False,readRect x):readRects xs
readRects _ = error "no parse"

limit :: Int
limit = 50

checkSize :: Rect -> Bool
checkSize (Rect (minx,maxx) (miny,maxy) (minz,maxz)) = all ((<=limit) . abs) [minx,maxx,miny,maxy,minz,maxz]

fixReactor :: String -> IO Int
fixReactor xs = sweep (-limit,-limit,-limit) ((-limit,limit),(-limit,limit),(-limit,limit))
                . filter (checkSize . snd) . reverse . readRects . lines <$> readFile xs

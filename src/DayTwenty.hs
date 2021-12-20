-- |

module DayTwenty (enhanceFile) where

import Data.Bool
import Data.Bits
import Data.Set (Set,member)
import qualified Data.Set as S
import Data.Vector (Vector,(!))
import qualified Data.Vector as V

type Pos = (Int,Int)
type Image = Set Pos

takeInt :: Int -> [Bool] -> Int
takeInt 0 _ = 0
takeInt _ [] = 0
takeInt i (x:xs) = (bool 0 1 x `shiftL` (i-1)) + takeInt (i-1) xs

getBounds :: Image -> (Pos,Pos)
getBounds img = ((minx,miny),(maxx,maxy))
  where
    minx = S.findMin $ S.map fst img
    maxx = S.findMax $ S.map fst img
    miny = S.findMin $ S.map snd img
    maxy = S.findMax $ S.map snd img

enhancePoint :: Vector Bool -> (Pos,Pos) -> Bool -> Image -> Image -> Pos -> Image
enhancePoint algo ((minx,miny),(maxx,maxy)) outer ogimg img (x,y) = if algo ! i then S.insert (x,y) img else S.delete (x,y) img
  where
    i = takeInt 9 [if (a < minx || b < miny) || (a > maxx || b > maxy) then outer else (a,b) `member` ogimg | b <- [(y-1)..(y+1)], a <- [(x-1)..(x+1)]]

enhance :: Vector Bool -> Image -> Bool -> Image
enhance algo img outer = foldl (enhancePoint algo ((minx,miny),(maxx,maxy)) outer img) img [(x,y) | y <- [(miny-1)..(maxy+1)],x <- [(minx-1)..(maxx+1)]]
  where
    ((minx,miny),(maxx,maxy)) = getBounds img

readAlgo :: String -> [Bool]
readAlgo [] = []
readAlgo ('#':xs) = True:readAlgo xs
readAlgo (_:xs) = False:readAlgo xs

readImg :: Pos -> [String] -> Image
readImg _ [] = S.empty
readImg (_,y) ([]:ys) = readImg (0,y+1) ys
readImg (x,y) (('#':xs):ys) = S.insert (x,y) $ readImg (x+1,y) (xs:ys)
readImg (x,y) ((_:xs):ys) = readImg (x+1,y) (xs:ys)

enhanceFile :: String -> IO Int
enhanceFile xs = do
  (algoline:(_:imglines)) <- lines <$> readFile xs
  let algo = V.fromList $ readAlgo algoline
      img = readImg (0,0) imglines
  print $ getBounds img
  return . S.size . foldl (enhance algo) img $ take 50 $ if algo ! 0 then cycle [False,True] else repeat False

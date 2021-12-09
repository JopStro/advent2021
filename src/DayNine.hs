-- |

module DayNine where
import Data.Vector (Vector, fromList, (!), (!?))
import qualified Data.Vector as V
import Data.Set (Set, member)
import qualified Data.Set as S
import Data.Maybe
import Data.Char (digitToInt)
import Data.List (sortBy)

calculateRisk :: Int -> Int -> Vector (Vector Int) -> Int
calculateRisk x y v | y >= V.length v           = 0
                    | x >= V.length (v ! y)     = calculateRisk 0 (y+1) v
                    | isLow                     = 1 + val + calculateRisk (x+2) y v
                    | otherwise                 = calculateRisk (x+1) y v
  where
    val = v ! y ! x
    up = fromMaybe 9 ((v !? (y-1)) >>= (!? x))
    down = fromMaybe 9 ((v !? (y+1)) >>= (!? x))
    left = fromMaybe 9 (v ! y !? (x-1))
    right = fromMaybe 9 (v ! y !? (x+1))
    isLow = val < up && val < down && val < left && val < right

getBasins :: Int -> Int -> Vector (Vector Int) -> [Int]
getBasins x y v | y >= V.length v           = []
                | x >= V.length (v ! y)     = getBasins 0 (y+1) v
                | isLow                     = S.size (findBasin x y v mempty) : getBasins (x+2) y v
                | otherwise                 = getBasins (x+1) y v
  where
    val = v ! y ! x
    up = fromMaybe 9 ((v !? (y-1)) >>= (!? x))
    down = fromMaybe 9 ((v !? (y+1)) >>= (!? x))
    left = fromMaybe 9 (v ! y !? (x-1))
    right = fromMaybe 9 (v ! y !? (x+1))
    isLow = val < up && val < down && val < left && val < right

findBasin :: Int -> Int -> Vector (Vector Int) -> Set (Int,Int) -> Set (Int,Int)
findBasin x y v found | val == 9 || member (x,y) found  = found
                      | otherwise                       = findBasin x (y-1) v
                                                          $ findBasin x (y+1) v
                                                          $ findBasin (x-1) y v
                                                          $ findBasin (x+1) y v newSet
  where
    val = fromMaybe 9 (v !? y >>= (!? x))
    newSet = S.insert (x,y) found



analyse :: String -> IO (Int, Int)
analyse xs = do
  heightmap <- fromList . map (fromList . map digitToInt) . lines <$> readFile xs
  return (calculateRisk 0 0 heightmap, product $ take 3 $ sortBy (flip compare) $ getBasins 0 0 heightmap)

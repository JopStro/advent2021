-- |

module DayThirteen where
import Data.Bifunctor
import Data.Set (Set, fromList, toList, size)
import qualified Data.Set as S
import Data.Tuple

data Line = X Int | Y Int
type Pos = (Int,Int)

fold :: Set Pos -> Line -> Set Pos
fold grid (X line) = S.map (\(x,y) -> if x > line then (line - (x - line),y) else (x,y)) grid
fold grid (Y line) = S.map (\(x,y) -> if y > line then (x,line - (y - line)) else (x,y)) grid

readPos :: String -> Pos
readPos xs = read ("("++xs++")")

readLine :: String -> Line
readLine xs = case drop 11 xs of
                ('x':'=':ns) -> X (read ns)
                ('y':'=':ns) -> Y (read ns)
                _ -> error "Invalid file format"

drawPaper :: Pos -> [Pos] -> IO ()
drawPaper _ [] = putChar '\n'
drawPaper (x,y) ((x0,y0):ps) = (if y == y0
                                then putStr (replicate (x0 - x - 1) ' ' ++ "#")
                                else putStr (replicate (y0 - y) '\n' ++ replicate x0 ' ' ++ "#")) >> drawPaper (x0,y0) ps

foldPaper :: String -> IO Int
foldPaper xs = do
  (paper,folds) <- bimap (fromList . map readPos) (map readLine) . break ((=="fold") . take 4) . filter (not . null) . lines <$> readFile xs
  drawPaper (0,0) $ map swap $ toList $ S.map swap $ foldl fold paper folds
  return $ size $ fold paper (head folds)

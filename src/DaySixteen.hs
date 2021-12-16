-- |

module DaySixteen (readPacket) where
import Data.Bool
import Data.Bits
import Data.Bifunctor
import Data.List
import Data.Function

hexToList :: Char -> [Bool]
hexToList x = case x of
                '0' -> map bitToBool "0000"
                '1' -> map bitToBool "0001"
                '2' -> map bitToBool "0010"
                '3' -> map bitToBool "0011"
                '4' -> map bitToBool "0100"
                '5' -> map bitToBool "0101"
                '6' -> map bitToBool "0110"
                '7' -> map bitToBool "0111"
                '8' -> map bitToBool "1000"
                '9' -> map bitToBool "1001"
                'A' -> map bitToBool "1010"
                'B' -> map bitToBool "1011"
                'C' -> map bitToBool "1100"
                'D' -> map bitToBool "1101"
                'E' -> map bitToBool "1110"
                'F' -> map bitToBool "1111"
                _ -> error "Not a hex digit"

bitToBool :: Char -> Bool
bitToBool '0' = False
bitToBool '1' = True
bitToBool _ = error "not a bit"

intBool :: Bool -> Int
intBool = bool 0 1

chopInt :: Int -> [Bool] -> (Int,[Bool])
chopInt i xs = (takeInt i xs, drop i xs)

takeInt :: Int -> [Bool] -> Int
takeInt 0 _ = 0
takeInt _ [] = 0
takeInt i (x:xs) = (intBool x `shiftL` (i-1)) + takeInt (i-1) xs

handlePacket :: [Bool] -> (Int,Int,[Bool])
handlePacket [] = (0,0,[])
handlePacket xs = handlePacket' ver t content
  where
    (ver,(t,content)) = second (chopInt 3) $ chopInt 3 xs

handlePacket' :: Int -> Int -> [Bool] -> (Int,Int,[Bool])
handlePacket' ver 4 content = (ver,takeInt (length value) value,rest)
  where
    (value,rest) = parseType4 content
handlePacket' ver t (False:xs) = (ver + vsum,val,rest)
  where
    (len,slen) = chopInt 15 xs
    (content,rest) = splitAt len slen
    f = functionFromType t
    (vsum,val) = foldl1 (\(ver0,val0) (ver1,val1) -> (ver0 + ver1, val0 `f` val1))
                 $ unfoldr (\ys -> if null ys then Nothing else
                                     handlePacket ys & \(v, n, r) -> Just ((v,n),r)) content
handlePacket' ver t (True:xs) = (ver+vsum,val,rest)
  where
    (q,sq) = chopInt 11 xs
    f = functionFromType t
    (vsum,val,rest) = foldl1 (\(ver0,val0,_) (ver1,val1,r) -> (ver0 + ver1, val0 `f` val1,r))
                      $ take q $ unfoldr (\ys -> if null ys then Nothing else
                                                   handlePacket ys & \(v, n, r) -> Just ((v,n,r),r)) sq
handlePacket' _ _ _ = error "Unreachable"

functionFromType :: Int -> (Int -> Int -> Int)
functionFromType t = case t of
                       0 -> (+)
                       1 -> (*)
                       2 -> min
                       3 -> max
                       5 -> (\x y -> intBool (x > y))
                       6 -> (\x y -> intBool (x < y))
                       7 -> (\x y -> intBool (x == y))
                       _ -> error "Invalid operator"

parseType4 :: [Bool] -> ([Bool],[Bool])
parseType4 [] = ([],[])
parseType4 (False:xs) = splitAt 4 xs
parseType4 (True:xs) = first (segment++) $ parseType4 rest
  where
    (segment,rest) = splitAt 4 xs

readPacket :: String -> IO (Int,Int)
readPacket xs = do
  (versionSum,val,_) <- handlePacket . concatMap hexToList . head . lines <$> readFile xs
  return (versionSum,val)

-- |

module DayTen where
import Data.Either
import Data.List

type Stack = [Char]

close :: Char -> Maybe Char
close '(' = Just ')'
close '[' = Just ']'
close '{' = Just '}'
close '<' = Just '>'
close _   = Nothing

checkSyntax :: String -> Stack -> Either Char String
checkSyntax [] xs = Right xs
checkSyntax (x:xs) stack = case close x of
                             Just c -> checkSyntax xs (c:stack)
                             Nothing -> if x == head stack then
                                          checkSyntax xs $ tail stack
                                        else
                                          Left x

score :: Char -> (Int, Integer)
score ')' = (3,1)
score ']' = (57,2)
score '}' = (1197,3)
score '>' = (25137,4)
score _   = (0,0)

calculateScore :: [Char] -> Int
calculateScore = foldl (\total x -> total + fst (score x)) 0

calculateCompletionScore :: [String] -> Integer
calculateCompletionScore xs = lineScores !! (length lineScores `div` 2)
  where
    lineScores = sort $ map (foldl (\total x -> total * 5 + snd (score x)) 0) xs

validateFile :: String -> IO (Int,Integer)
validateFile xs = do
  (corrupt, incomplete) <- partitionEithers . map (`checkSyntax` []) . lines <$> readFile xs
  return (calculateScore corrupt, calculateCompletionScore incomplete)

-- |

module DayTen where
import Data.Maybe
import Data.List
import Data.Map (Map, (!), fromList)

type Stack = [Char]

opening :: [Char]
opening = "([{<"



closeMap :: Map Char Char
closeMap = fromList [ ('(', ')')
                    , ('[', ']')
                    , ('{', '}')
                    , ('<', '>')
                    ]

checkSyntax :: String -> Stack -> Maybe String
checkSyntax [] xs = Just xs
checkSyntax (x:xs) ys     | x `elem` opening = checkSyntax xs (closeMap ! x:ys)
checkSyntax (x:xs) (y:ys) | x == y           = checkSyntax xs ys
                          | otherwise        = Nothing

calculateScore :: [Char] -> Int
calculateScore = foldl (\total x -> total + (scores ! x)) 0
  where
    scores :: Map Char Int
    scores = fromList [ (')', 3)
                      , (']', 57)
                      , ('}', 1197)
                      , ('>', 25137)
                      ]

calculateCompletionScore :: [String] -> Int
calculateCompletionScore xs = lineScores !! (ceiling (fromIntegral (length xs) / 2 :: Double) - 1)
  where
    lineScores = sort $ map (foldl (\total x -> (total * 5) + (scores ! x)) 0) xs
    scores :: Map Char Int
    scores = fromList [ (')', 1)
                      , (']', 2)
                      , ('}', 3)
                      , ('>', 4)
                      ]

validateFile :: String -> IO Int
validateFile xs = calculateCompletionScore . mapMaybe (`checkSyntax` []) . lines <$> readFile xs

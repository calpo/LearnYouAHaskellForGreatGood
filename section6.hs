{-# OPTIONS -Wall -Werror #-}

--import qualified Data.List as L
import Data.List

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
--wordNums xs = map (\ws -> (head ws, length ws)) . group . sort $ words xs
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (isPrefixOf needle) (tails haystack)

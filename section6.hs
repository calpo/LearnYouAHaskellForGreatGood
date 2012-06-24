-- {-# OPTIONS -Wall -Werror #-}

import Data.List
import Data.Char
import qualified Data.Map as M

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
--wordNums xs = map (\ws -> (head ws, length ws)) . group . sort $ words xs
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (isPrefixOf needle) (tails haystack)

encode :: Int -> String -> String
--encode offset msg = map (\c -> chr ((ord c) + offset)) msg
--encode offset msg = map (\c -> chr $ ord c + offset) msg
encode offset = map $ chr . (+offset) . ord

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

findTo40 :: Maybe Int
findTo40 = find (\x -> digitSum x == 40) [1..]

findTo :: Int -> Maybe Int
findTo num = find (\x -> digitSum x == num) [1..]

phoneBook' :: [(String,String)]
phoneBook' = [
         ("betty", "111-222")
        ,("hoge", "333-444")
        ,("fuga","555-666")
    ]

findByKey'' :: Eq k => k -> [(k,v)] -> v
findByKey'' key = snd . head . filter (\(k,_) -> key == k)

findByKey :: Eq k => k -> [(k,v)] -> Maybe v
findByKey _ [] = Nothing
findByKey key ((k,v):xs)
    | k == key  = Just v
    | otherwise = findByKey key xs

findByKey' :: Eq k => k -> [(k,v)] -> Maybe v
findByKey' key xs = foldr
                        (\(k,v) acc -> if k == key then Just v else acc)
                        Nothing
                        xs


phoneBook :: M.Map String String
phoneBook = M.fromList $
    [("betty", "111-222")
    ,("hoge", "333-444")
    ,("fuga","555-666")
    ]

string2digit :: String -> [Int]
--string2digit xs = map digitToInt $ filter isDigit xs
string2digit = map digitToInt . filter isDigit

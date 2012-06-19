{-# OPTIONS -Wall -Werror #-}

maximum' :: Ord a => [a] -> a
maximum' [] = error "The list is empty."
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

--replicate' :: Int -> a -> [a]
--replicate' 0 _ = []
--replicate' num x = x:(replicate' (num - 1) x)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n -1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0            = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

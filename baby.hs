--doubleMe :: Num a => a -> a
doubleMe :: Int -> Int
doubleMe x = x + x

--doubleUs :: Num a => a -> a -> a
doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

--doubleSmallNumber :: Num a => a -> a
doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
						then x
						else x*2

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = doubleSmallNumber x + 1

boomBang :: [Int] -> [[Char]]
boomBang xs = [ if x < 10 then "Boom!" else "Ahh!" | x <- xs, odd x, x < 20]

length' :: [t] -> Int
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triples' :: Int -> [(Int, Int, Int)]
triples' x = [ (a, b, c) | c <- [1..x], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2 ]

--addThree :: Num a => a -> a-> a-> a
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

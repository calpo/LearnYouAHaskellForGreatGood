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
factorial 0 = 1
factorial n = n * factorial (n - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

fizzbuzz' :: Int -> [Char]
fizzbuzz' 2 = "fizz"
fizzbuzz' 3 = "buzz"
fizzbuzz' 4 = "fizz"
fizzbuzz' 6 = "fizzbuzz"
fizzbuzz' x = show x

-- fizz buzz challenge
fizz :: Int -> String
fizz x = if x `mod` 3 == 0 then "fizz" else ""

buzz :: Int -> String
buzz x = if x `mod` 4 == 0 then "buzz" else ""

fandb :: Int -> String
fandb x = fizz x ++ buzz x

fandb' :: Int -> String
fandb' x = if fandb x == "" then show x else fandb x

fizzbuzz :: Int -> [String]
fizzbuzz x = [ fandb' a | a <- [1..x] ]

fibonatch 0 = 1
fibonatch 1 = 1
fibonatch x = fibonatch (x - 2) + fibonatch (x - 1)
fibonatchList xs = [ fibonatch x | x <- [1..xs] ]

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
--charName x = [x]

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
--addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " " ++ show y
tell (x:y:_) = "The list has many elements: " ++ show x ++ " " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "all:" ++ all ++ " first:" ++ [x]

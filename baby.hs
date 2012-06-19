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

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists."
                       (x:_) -> x

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " " ++ show y
tell (x:y:_) = "The list has many elements: " ++ show x ++ " " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "all:" ++ all ++ " first:" ++ [x]

max' :: Ord a => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: Ord a => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a < b     = LT
    | otherwise = GT

badGreeting :: String
badGreeting = "huh.."

niceGreeting :: String
niceGreeting = "Hello"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan."
greet name = badGreeting ++ " " ++ name ++ "."

greet' :: String -> String
greet' name = case name of  "Juan" -> niceGreeting ++ " Juan."
                            name -> badGreeting ++ " " ++ name ++ "."
    where   niceGreeting = "Hello"
            badGreeting = "huh.."

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skiny  = show bmi ++ " You're underweight, you emo, you!"
    | bmi <= normal = show bmi ++ " You're supposedly normal."
    | bmi <= fat    = show bmi ++ " You're fat."
    | otherwise     = show bmi ++ " You're flesh meet."
    where   bmi = weight / height ^ 2
            (skiny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where   bmi w h = w / h ^ 2
-- calcBmis (zip [50,55..80] (replicate 7 1.73))

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 20]

describeList :: [a] -> String
describeList ls = "The list is "
                    ++ case ls of   [] -> "empty"
                                    [x] -> "single"
                                    xs -> "long"

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where   what [] = "empty"
            what [x] = "single"
            what xs = "long"

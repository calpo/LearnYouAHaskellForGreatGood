doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

--doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
doubleSmallNumber' x = doubleSmallNumber x + 1

boomBang xs = [ if x < 10 then "Boom!" else "Ahh!" | x <- xs, odd x, x < 20]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

--triples' x = [ (a, b, c) | c <- [1..x], a <- [1..c], b <- [1..a] ]
--triples' x = [ (a, b, c) | c <- [1..x], a <- [1..x], b <- [1..x], c < a + b, a <= c, b <= c, a <= b ]
triples' x = [ (a, b, c) | c <- [1..x], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2 ]

--addThree Int -> Int -> Int -> Int
addThree x y z = x + y + z

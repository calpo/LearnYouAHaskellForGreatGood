doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

--doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
doubleSmallNumber' x = doubleSmallNumber x + 1

boomBang xs = [ if x < 10 then "Boom!" else "Ahh!" | x <- xs, odd x, x < 20]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

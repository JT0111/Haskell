--Lab2 found on 2nd year module page

--Excercise 1
last' :: [a] -> a
last' (x:[]) = x
last' x = last' (tail x) 
--alternatively: last' (_:xs) = last' xs


--Exercise 2
--using head and tail:
third :: [a] -> a
third x = head (tail (tail x))

--using list indexing 
third' :: [a] -> a
third' xs = xs !! 2

--using pattern matching
third'' :: [a] -> a
third'' (_:_:a:_) = a


--Exercise 3
--using if-then-else
safetail :: [a] -> [a]
safetail xs = (if (null xs) then xs else (tail xs)) 

--using guarded equations
safetail' :: [a] -> [a]
safetail' xs
    | null xs = []
    | otherwise = tail xs

--using pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs


--Excercise 4
halve :: [a] -> ([a], [a])
halve xs = ( take half xs, drop half xs)
    where half = div (length xs) 2


--Exercise 5
enc :: Int -> String -> String
enc a xs = [ toEnum((fromEnum x)+a) | x<-xs]

encrypt :: Int -> String -> (String , String -> String)
encrypt a xs = (enc a xs, dec)
    where dec x = enc (a*(-1)) x


--Exercise 6
luhnDouble :: Int -> Int
luhnDouble x
    | x >= 5 = 2 * x - 9
    | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d
    | mod totalSum 10 == 0 = True
    | otherwise = False
    where totalSum = sum [luhnDouble a, b, luhnDouble c, d]



--Exercise A1
histogram :: Int -> [Int] -> [Int]
histogram a xs = [count x | x <- [1 .. ((maximum xs) + a)], mod x a == 0]
    where count num = length [z | z <- xs, z < num, z >= num-a]


--Exercise A2
approxSqrt :: Double -> Double -> Double
approxSqrt d e = head [x | x <- [guess i | i <- [0..]], (x^2 <= d && (x^2) + (e^2) + (2 * x * e) > d) || (x^2 > d && (x^2) + (e^2) - (2 * x * e) < d)]
    where
        guess 0 = 1
        guess x = ((guess (x - 1) + d) / guess (x - 1)) / 2
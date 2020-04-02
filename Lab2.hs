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

--using list indexing - will be done later !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--third' :: [a] -> a
--third' = ...


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
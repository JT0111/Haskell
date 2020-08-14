--Lab1 found on 2nd year module page

--Excercise 3 (1 and 2 are not coding exercizes)
--"Define a function product that produces the product of a list of numbers"

--recursive version
product' :: Num a => [a] -> a
product' (x:xs) = if (null xs) then x else x * product' xs --I could add a base case for empty list above but don't really see a point

--using foldl
product'' xs = foldl (\acc x -> acc * x) 1 xs --acc accumulates the result

--Exercise 4
--quicksort [] = []
--quicksort (x:xs) = quicksort ls ++ [x] ++ quicksort rs
--                   where 
--                     ls = [ a | a <- xs , a <= x ]
--                     rs = [ a | a <- xs , a > x ]
--Modify this definition to create a function quicktros that produces a sorted version of the argument list in descending order. quicktros
quicktros :: Ord t => [t] -> [t] --transforms array of orderable elements to array of erderable elements
quicktros [] = []
quicktros (x:xs) = quicktros ls ++ [x] ++ quicktros rs
                   where 
                     ls = [ a | a <- xs , a >= x ] 
                     rs = [ a | a <- xs , a < x ]


--Exercise 7
--The following script contains at least three syntactic errors. Copy the code in to a script, 
--correct all errors and check that your solution loads correctly using GHCi.
--N = a 'div' length xs
--	    where 
--	       a = 10 
--	      xs = [1,2,3,4,5]

n :: Int --div can only return an Integral number
n = div a (length xs)
	    where a = 10
	          xs = [1,2,3,4,5]



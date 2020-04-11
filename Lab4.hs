--Exercise 1 (not finished)
--Decide if all elements of a list satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x acc -> if f x then acc else False) True xs

--Decide if any element of a list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr (\x acc -> if f x then True else acc) False xs

--Select the initial elements from a list while they satisfy a predicate
--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' p xs = 

--Exercise 2
dec2Int :: [Int] -> Int
dec2Int xs = foldl (\acc x -> (10 * acc) + x) 0 xs

--Exercise 4

--int2bin :: Integer -> [Integer]
int2bin x   
        | x == 1 = [1]
        | x == 0 = []
        | otherwise = (mod x 2) : int2bin (div x 2) --doesn't work yet, you have to turn the list and ++ doesn't work (idk why)

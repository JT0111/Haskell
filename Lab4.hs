--Exercise 1
--Decide if all elements of a list satisfy a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = foldr (\x acc -> if f x then acc else False) True xs

--Decide if any element of a list satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' f xs = foldr (\x acc -> if f x then True else acc) False xs

--Select the initial elements from a list while they satisfy a predicate
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile'   
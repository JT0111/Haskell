--Exercise 1
exp' :: Int -> Int
exp' n = sum [x^2 | x <- [0..n], mod x 2 == 1] + sum [x^3 | x <- [0..n], mod x 2 == 0] --there should be 100 accept n but I like it better

--Exercise 2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0, 1 .. m], y <- [0, 1 .. n]]

square :: Int -> [(Int, Int)]
square n = [(x, y) | x <- [0 .. n], y <- [0..n], x/=y ]

--Exercise 3
replicate' :: Int -> a -> [a]
replicate' a x = [f n | n <- [1..a]]
    where f n = x

--Exercise 4
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x, y, z) | x<-[1..n], y<-[1..n], z<-[1..n], ((x^2) + (y^2)) == (z^2)]

--Exercise 5
perfect :: Int -> [Int]
perfect n = [x | x<-[1..n], x == sum [z | z<-[1..x-1], mod x z == 0]]

--Exercise 6
find :: Eq a => a -> [ (a,b)] -> [b]
find k t = [ v | (k',v) <- t, k==k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..(length xs)])

--Exercise 7
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [ x*y | (x, y) <- (zip xs ys)]

--Exercise 8

--Exercise A3
--longestCommonSubsequence :: Eq a => [[a]] -> [a]
--longestCommonSubsequence xs = [x | x <- xs]
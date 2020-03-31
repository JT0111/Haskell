rightTriangles = [(a, b, c) |  a <- [1..10],  b <- [1..a], c <- [1..100], a^2 + b^2 == c^2, a + b > c ]

ifRight :: (Num a, Ord a) => (a, a, a) -> Bool
ifRight (a, b, c) = if a*a + b*b == c*c && a + b > c then True else False

lotsOfHearts = take 100(cycle "<3")
sameAsAboveButAsASetOfStrings = replicate 50 "<3"

jts :: Integral a => [a] -> [[Char]]
jts xs = [ if x < 10 then "J" else "T" | x <- xs, odd x] 

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']] 
 
evenSubsetsMembers :: [[Int]] -> [[Int]]
evenSubsetsMembers xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

numbers = zip [1, 2, 3] ["one", "two", "three"]

increasedLength :: (Foldable t, Fractional a1) => t a2 -> a1
increasedLength xs = fromIntegral (length xs) + 3.2
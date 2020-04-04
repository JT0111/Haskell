rightTriangles = [(a, b, c) |  a <- [1..10],  b <- [1..a], c <- [1..100], a^2 + b^2 == c^2, a + b > c ] -- .. means that we take everything in between

ifRight :: (Num a, Ord a) => (a, a, a) -> Bool --not sure why the "Ord" is here, does it mean that not all "Num"s are orderable?
ifRight (a, b, c) = if a*a + b*b == c*c && a + b > c then True else False

--I have to check the type
lotsOfHearts = take 100(cycle "<3") --cycle makes infinite numbers of copies, take takes a given number of first elements from list
sameAsAboveButAsASetOfStrings = replicate 50 "<3" --this works just the same

jts :: Integral a => [a] -> [[Char]]
jts xs = [ if x < 10 then "J" else "T" | x <- xs, odd x] --if..then...else syntax

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]  --elem x xs checks if x is in xs
 
evenSubsetsMembers :: [[Int]] -> [[Int]]
evenSubsetsMembers xxs = [ [ x | x <- xs, even x ] | xs <- xxs] --nested list comprehension example 

numbers = zip [1, 2, 3] ["one", "two", "three"] --zip puts together two lists dropping the last elements of the longer one

increasedLength :: (Foldable t, Fractional a1) => t a2 -> a1 --not sure why it's "foldable", guess it's because there are more than one structures with length
increasedLength xs = fromIntegral (length xs) + 3.2

iflessthen :: (Ord a, Num a) => a -> String -- can be also [Char]
iflessthen 0 = "number is 0" --checking goes in order up -> down so this is checked first
iflessthen num  --here I start using guards because I'm not interested in exact numbers (more like ranges)
    | num < 7 = "Number is < 7"  
    | num < 15 = "Number is in range <7, 15)"   
--  | otherwise   = "Number is over 14" --otherwise always returns true
iflessthen x = "Number is over 14" --just checked if that's an option too - I know it's ugly

initials :: [Char] -> [Char] -> [Char]  --function takes 2 strings and produces string as an output
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  --f ist the first element from the [Char] firsname
          (l:_) = lastname  --this "where" isn't actually needed here
--alternative verision:
initials' :: [Char] -> [Char] -> [Char]
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  -- function taks array of touples (weight, hight) and returns an array of bmis
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 -- thos function is just defined within a function so it can be re-written as:

calcBmis' xs = [w/h^2 | (w, h) <- xs] --types are the same
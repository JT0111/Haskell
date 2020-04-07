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

--function generating infinite sequence of fibonacci numbers using recursion
--there is a simpler function that I'll find later
fib = 1:1:(map (uncurry (+)) (zip fib (tail fib))) --uncarry (+) adds the zipped numbers so :: NUm a => [(a, a)] -> [a] (check it), not sure what map does

--simple function that can be rewritten with a carried function
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  

--carried function that does exactly the same thing
compareWithHundred' :: (Num a, Ord a) => a -> Ordering --it has to be this type because of 100
compareWithHundred' = compare 100  -- when we "add" the argument on the command line it would be the same as calling compare 100 

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) --part in () must lack a parameter on one "end" and the argument is just inserted ther

applyTwice :: (a -> a) -> a -> a  -- takes a function "(a -> a)" and a parameter "a" and returns a result of the same type as a
applyTwice f x = f (f x)  -- given function is applied twice on x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  --f is a function of type (a->b->c) so neither the 2 parameters nor the result must be the same type
zipWith' _ [] _ = [] --if any of the lists is empty return empty set
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  --otherwise apply f in elements on coresponding positions and return to the result list

flip' :: (a -> b -> c) -> (b -> a -> c)  --takes and returns a function, types of functions parameeters swaps but the return type stayes the same 
flip' f y x = f x y  

--map implementation
map' :: (a -> b) -> [a] -> [b]  --takes function and a list as parameter and returns a new list
map' _ [] = []  --emoty list is still emoty as the number of elements doesn't change
map' f (x:xs) = f x : map f xs --[f x | x <- x:xs] would do the same thing


filter' :: (a -> Bool) -> [a] -> [a]   
filter' _ [] = []  
filter' p (x:xs)   --p is usuallu in (), e.g. (>3)
    | p x       = x : filter' p xs  --if the first element satisfies p it is added to the new list
    | otherwise = filter' p xs  --otherwise it's not
    --then recursively function checks 2nd, 3rd and next elements

--example of filter:
largestDivisible :: (Integral a) => a --there are no paramethers so there is just one a  
largestDivisible = head (filter p [100000,99999..]) --[10000, 99999 ..] assures that the head is the largest
    where p x = x `mod` 3829 == 0  --elements must be divisable by 3829

firstWord :: [Char] -> [Char]
firstWord = takeWhile (/=' ') --takes all elements before the first ' '

flip'' :: (a -> b -> c) -> b -> a -> c --flip function using lambda expression, looking at notation below: f::(a -> b -> c), x :: b,  y :: a and final result :: c
flip'' f = \x y -> f y x  --f is a function taking 2 parameters (x and y) that are also passed to "flip''"
--'\' is used for lambda expression, everything between '\' and "->" is a parameter, after are results

sum' :: (Num a) => [a] -> a --takes a list of numbers and returns a single one (e.g. folds the list to the single vale of its sum)
sum' xs = foldl (\acc x -> acc + x) 0 xs  --"\acc x -> acc + x" means that the next 2 parameters will be summed,
-- as acc is updated after each operation (because of foldl[eft]), it will just keep getting bigger until the list is empty 
--foldl returns the acc value (e.g. the sum)

sum'' :: (Num a) => [a] -> a --the same as above, just shorter because it uses a fact that functions can be carried 
sum'' = foldl (+) 0  --sum'' takes some list, e.i. xs as parameter so it can be rewritten as: "sum'' xs = foldl (+) 0 xs"
-- this is more similar to example above, the same rule applies to "(+)" - as (+) :: Num a => a -> a -> a it will be carried to the next parameters just as if it was "((+) acc x)"

map'' :: (a -> b) -> [a] -> [b]  --below: f :: (a -> b), xs :: [a] and result :: [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs --apply the f function to every element of xs, starting from the last, and make it a head of acc
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--Soooo... everything is swapped, acc is (at first) [], and x is every element of xs starting from the last one

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  --example of foldr1 -> doesnt care about acc value, just takes the first checked elements' one

currentMaximum :: (Num a, Ord a) => [a] -> [a]
currentMaximum = scanl1 (\acc x -> if x > acc then x else acc) --scan(l/r)( /1) works like fold but gives a list of all acc states

funnyFunction :: [(Int -> Int)] -> [Int]
funnyFunction xs = map ($ 3) xs--maps all half-filled functions with a 3 so we get a "normal" list, "$" is a sign of function taking place, xs can be e.i. = [(4+), (10*), (^2), sqrt]

--Function composition allows to ommit () as it's right-associative so "f . g x" is the same as (in math) f(g(x))
oddSquareSum :: Integer  --random example
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  --takes infinite list of numbers, maps them to it's square, filters the even ones out and stops when reaches 10000, then gets a sum
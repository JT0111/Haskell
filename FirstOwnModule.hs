--module implementing sorting algorithms
module FirstOwnModule 
( quicksort
, quicktros
, mergeSort
, selectionSort
) where

--increasing order
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort greater
                where   
                    smaller = [a | a <- xs, a < x]
                    greater = [a | a <- xs, a >= x]

--decreasing order
quicktros :: (Ord a) => [a] -> [a]
quicktros [] = []
quicktros (x:xs) = quicktros greater ++ [x] ++ quicktros smaller
                where
                    smaller = [a | a <- xs, a <= x]
                    greater = [a | a <- xs, a > x]

--merge sort (only increasing order)
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys
    | null xs = ys
    | null ys = xs
    | head xs > head ys = (head ys):(merge xs (tail ys))
    | otherwise = (head xs):(merge ys (tail xs))

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
    where half = div (length xs) 2

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
    where  (x1, x2) = (halve xs) 

--Selection sort, again, only increasing, probably could be done with just 2 traverses 
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = [];
selectionSort (x:[]) = [x]
selectionSort xs = ys ++ selectionSort[x | x <- xs, x /= m]
    where   m = minimum xs
            ys = [y | y<-xs, y == m]

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
           --Show and read alows it to be converted grom/to strings
           --Eq enables "compoare"
           --Enum means that this type has  predecessors and successors
           --Bounded means that there is lowest and higher possible value (minBound, maxBound functions)
           --Ord gives us option to call < > and sort
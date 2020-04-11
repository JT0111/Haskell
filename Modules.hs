import Data.List --syntax for importing modules is "import name_of_module ([sometimes] specific fuctions we want to include separated by commas)"
-- import Data.List (nub, sort) imports just nub and sort
-- import Data.List hiding (nub) impots Data.List without the nub function - usefull if you want to write your own implementation  
--modules can be imported in ghci using "":m + module_name"

import qualified Data.Map as M   --"qualified" means we need to type name of this module before every function from it, as M changes this name
-- so, if we want to use "filter" from this module we would write "M.filter", "filter" would mean a function from "Prelude" module

--more modules are here: https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
--library search: https://hoogle.haskell.org/

--more on Data.List:

--num - removes duplicates
numUniques :: (Eq a) => [a] -> Int  --eq because we have to be able to spot the duplicates
numUniques = length . nub  --'.' takes everything after it into () so it's the same as length (nub xs) but allows carrying the function

--transpose a list of lists (treats it as a matrix)
sumPolynomials :: (Num a) => [[a]] -> [a]
sumPolynomials xxs = map sum $ transpose xxs --takes polynomials described as lists and produces a list of the sum-polynomial
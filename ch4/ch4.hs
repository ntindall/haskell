-- p 85


import Data.Char

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

-- this is tail recursion, slowly accumulate the result in acc

-- squares every element in an array
-- void square(double *out, const double *in, size_t length)
-- {
--   for (size_t i = 0; i < length; i++) { 
--     out[i] = in[i] * in[i];
--   }
-- }

square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square []     = []

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
uppercase []     = []

-- map takes a function and applies to every element of a list, returning
-- a new list constructed from the results of these applications

square2 xs = map squareOne xs
  where squareOne x = x * x

upperCase2 xs = map toUpper xs

-- map :: (a -> b) -> [a] -> [b]

-- The signature tells us that map takes two arguments. The first is a function
-- that takes a value of one type, a, and returns a value of another type, b.
-- Because map takes a function as an argument, we refer to it as a higher-order
-- function. (In spite of the name, there’s nothing mysterious about higher-
-- order functions; it’s just a term for functions that take other functions as
-- arguments, or return functions.) Since map abstracts out the pattern common
-- to our square and upperCase functions so that we can reuse it with less
-- boilerplate, we can look at what those functions have in common and figure
-- out how to implement it ourselves:

oddList :: [Int] -> [Int]
oddList (x:xs) | odd x     = x : oddList xs
               | otherwise = oddList xs
oddList _                  = []

-- filter :: (a -> Bool) -> [a] -> [a]

mySum xs = helper 0 xs
  where helper acc (x:xs) = helper (acc + x) xs
        helper acc _      = acc

-- Our helper function is tail-recursive and uses an accumulator parameter, acc,
-- to hold the current partial sum of the list. As we already saw with asInt,
-- this is a “natural” way to represent a loop in a pure functional language.

-------------------------------------------------------------------------------

-- foldl :: (a -> b -> a) -> a -> [b] -> a

-- The foldl function takes a “step” function, an initial value for its
-- accumulator, and a list. The “step” takes an accumulator and an element from
-- the list and returns a new accumulator value. All foldl does is call the
-- “stepper” on the current accumulator and an element of the list, and then
-- passes the new accumulator value to itself recursively to consume the rest of
-- the list.

foldSum xs = foldl step 0 xs
  where step acc x = acc + x

niceSum xs = foldl (+) 0 xs

-- we're no longer using explicit recursion, because foldl takes care of that
-- for us! We've simplified our problem.

-- A reader with a little experience will have an easier time understanding the
-- use of a fold than code that uses explicit recursion. A fold isn't going to 
-- produce any surprises, but the behavior of a function that recurses
-- explicitly isn't immediately obvious. 

-- The counterpart to foldl is foldr, whch folds from the right of the list

-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- use foldl' to avoid space leaks

asInt_fold :: String -> Int
asInt_fold ('-':xs) = -(asInt_fold xs)
asInt_fold xs = foldl step 0 xs
                where step x y = 10 * x + (digitToInt y)

myConcat :: [a] -> [a] -> [a]
myConcat l1 l2 = foldr (:) l2 l1


myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = foldl (||) False (map f l)


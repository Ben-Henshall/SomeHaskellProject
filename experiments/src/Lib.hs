module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

lostNumbers = [4,8,15,16,23,42]

prependSmallCat = 'A':" SMALL CAT"
indexingBuscemi = "Steve Buscemi" !! 6 

exampleList = [5,4,3,2,1]

listHead = head exampleList
listTail = tail exampleList
listLast = last exampleList
listInit = init exampleList
listIsEmpty = null exampleList
listContains = 5 `elem` exampleList

texasRangeNumbers = [1..20]
texasRangeAlphabetUpper = ['A'..'Z']
texasRangeAlphabetLower = ['a'..'z']
texasRangeFloat = [0.1, 0.2..1]
texasRangeEvens = [0, 2..20]


isPrime x
    | x == 1 = False
    | x == 2 = True
    | x == 3 = True
    | x `mod` 2 == 0 = False
    | x `mod` 3 == 0 = False
    | otherwise = isOfForm6k x 5 2

isOfForm6k :: (Integral a) => a -> a -> a -> Bool  
isOfForm6k n i w
    | i * i > n = True
    | n `mod` i == 0 = False
    | otherwise = newRecursive
    where newRecursive = isOfForm6k n (i+w) (w-6)

isPrimeImported k = null [ x | x <- [2..isqrt k], k `mod`x  == 0]
isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

longNumberedList = [1..10000]
myPrimes = map isPrime longNumberedList
theirPrimes = map isPrime longNumberedList
primesComparison = myPrimes == theirPrimes

-- List comprehension
doubleListComp1 = [ x*2 | x <- [1..10]]
doubleListComp2 = [ x*2 | x <- [1..10], x*2 < 14]
-- `show` is something that has a string representation
primeListPredicates xs = [ if x < 10 then "Low prime " ++ show x else "high prime " ++ show x | x <- xs, isPrime x ]

-- factorial :: Integer -> Integer  
--factorial n = product [1..n]  

-- Pattern matching
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

-- Pattern matching + maybe introduction
head' :: [a] -> Maybe a  
head' [] = Nothing 
head' (x:_) = Just x

-- Guards
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!" 
    where bmi = weight / height ^ 2 
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0   

-- mappedBMIs = map bmiTell [(180, 87), (150, 65), (180, 120)]

-- Infix functions are surrounded in backticks
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT 


-- Let bindings
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- Can define functions in line
-- [let square x = x * x in (square 5, square 3, square 2)] 

-- Case expressions
altHead :: [a] -> a  
altHead xs = case xs of [] -> error "No head for empty lists!"  
                        (x:_) -> x  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

--- Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "test" 
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i ->  b -> [b]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' element (x:xs)
    | x == element = True
    | otherwise = element `elem'` xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


-- Partial Application
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

-- Infix partial application
divideByTen :: (Floating a) => a -> a  
-- Surround infix operator (/) in parenthesis and any missing params are supplied later
-- through partial application
divideByTen = (/10)  

-- Quicksort with filter
quicksortFilter :: (Ord a) => [a] -> [a]    
quicksortFilter [] = []    
quicksortFilter (x:xs) =     
    let smallerSorted = quicksortFilter (filter (<=x) xs)  
        biggerSorted = quicksortFilter (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

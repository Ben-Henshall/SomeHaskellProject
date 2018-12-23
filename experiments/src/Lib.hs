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
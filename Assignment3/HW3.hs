-- CSCI 450
-- Homework 3
-- Tam Nguyen

module HW3
where

import Data.Char
import Data.List
import Data.Maybe
-- CHAPTER 5 EXERCISES
-- 3.
-- Suppose you need a Haskell function sumSqNeg that takes a list of integers (type Integer)
-- and returns the sum of the squares of the negative values in the list.
-- Define the following Haskell functions. Use the higher order library functions
-- (from this chapter) such as map, filter, foldr, and foldl as appropriate:
-- a.
-- Function sumSqNeg1 that is backward recursive. (Use recursion directly. Do not use the 
-- list-folding Prelude functions such as foldr or sum)
sumSqNeg1 :: [Int] -> Int
sumSqNeg1 [] = 0
sumSqNeg1 (n:ns)
    | n >= 0             = sumSqNeg1 (ns)
    | length (n:ns) == 1 = n*n
    | otherwise          = n*n + sumSqNeg1 (ns)

-- b.
-- Function sumSqNeg2 that is tail recursive. (Use recursion directly. Do not use the
-- list-folding Prelude functions such as foldr or sum)
sumSqNeg2 :: [Int] -> Int
sumSqNeg2 [] = 0
sumSqNeg2 (n:ns) = sumIter (n:ns) 0
    where
        sumIter [] s        = s
        sumIter (x:xs) s
            | x >= 0        = sumIter xs s
            | otherwise     = sumIter xs (s+x*x)

-- c.
-- Function sumSqNeg3 that uses standard prelude functions such as map, filter, foldr, and foldl.
sumSqNeg3 :: [Int] -> Int
sumSqNeg3 [] = 0
sumSqNeg3 (n:ns) = sum (map (^2) (filter (<0) (n:ns)))

-- d.
-- Function sumSqNeg4 that uses list comprehension (Chapter 7).
sumSqNeg4 :: [Int] -> Int
sumSqNeg4 xs = sum [ x*x | x<-xs, x < 0 ]

-- 4.
-- Define a Haskell function:
-- total :: (Integer -> Integer) -> Integer -> Integer
-- so that total f n gives f 0 + f 1 + f 2 + ... + f n.
total :: (Integer -> Integer) -> Integer -> Integer
total f n
    | n == 0 = f 0
    | otherwise = f n + total f (n-1)

-- 5.
-- Define a Haskell function:
-- removeFirst :: (a -> Bool) -> [a] -> [a]
-- so that removeFirst p xs removes the first element of xs that does has the property p.
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst p [] = []
removeFirst p (x:xs)
    | p x       = xs
    | otherwise = x:removeFirst p xs

-- 6.
-- Define a Haskell function:
-- removeLast :: (a -> Bool) -> [a] -> [a]
-- so that removeLast p xs removes that last occurence of element of xs that has the property p.
-- How could you define it using removeFirst?
removeLast :: (a-> Bool) -> [a] -> [a]
removeLast p [] = []
removeLast p (x:xs) = reverse (removeFirst p (reverse(x:xs)))

-- 7.
-- Define a Haskell function map2 that takes a list of functions and a list of values
-- and returns the list of results of applying each function in the first list to the
-- corresponding value in the second list.
map2 :: [a -> b] -> [a] -> [b]
map2 _ [] = []
map2 [] _ = []
map2 (p:ps) (a:as) = (p a):(map2 ps as)

-- 8.
-- Define a Haskell function fmap that takes a value and a list of functions
-- and returns the list of results of applying each function to the argument value.
-- (For example, fmap 3 [((*) 2), ((+) 2)] yields [6,5].)
fmap' :: a -> [(a->b)] -> [b]
fmap' _ [] = []
fmap' x (f:fs) = (f x):(fmap' x fs)

-- 9.
-- Define a Haskell function composeList that takes a list of functions and composes them
-- into a single function. (Be sure to give the type signature.)


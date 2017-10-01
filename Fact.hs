{-  Factorial Functions (Int versions)
    Section 2.2 of Basic Haskell Functional Programming notes
    Also referenced in Chapter 3
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

    2017-01-27: Revised for module
    2017-02-03: Added fact6/factInter
    2017-08-25: Added fact4', fact4'', fact7..fact11
                Only fact1, fact2, fact3, fact4, fact5 in Chapter 2
-}

module Fact 
    ( fact1, fact2, fact3, fact4, fact4', fact5,   -- from Ch. 2
      fact6,                                       -- from Ch. 3
      fact4'', fact7, fact8, fact9, fact10, fact11 -- extra
    ) 
where

-- Using an if-then-else expression
fact1 :: Int -> Int 
fact1 n = if n == 0 then 
              1 
          else
              n * fact1 (n-1)

-- Using guards (evaluate top to bottom until success)
fact2 :: Int -> Int 
fact2 n 
  | n == 0    = 1 
  | otherwise = n * fact2 (n-1)

-- Using pattern matching (evaluate top to bottom until success)
fact3 :: Int -> Int 
fact3 0 = 1 
fact3 n = n * fact3 (n-1)

-- Using partial function (undefined for negative n)
fact4 :: Int -> Int 
fact4 n 
  | n == 0 =  1 
  | n >= 1 =  n * fact4 (n-1)
 
-- fact4 with explicit error
fact4' :: Int -> Int 
fact4' n 
  | n == 0 =  1 
  | n >= 1 =  n * fact4' (n-1)
  | otherwise = error "fact4' called with negative argument"

-- fact4 with mix of patterns and guards, explicit error
fact4'' :: Int -> Int 
fact4'' 0          =  1 
fact4'' n | n >= 1 =  n * fact4'' (n-1)
fact4'' n          = error "fact4'' called with negative argument"

-- Was valid for older Haskell standard, but not for Haskell
-- 2010.  (n+k) patterns were removed from the language.
-- fact5 :: Int -> Int 
-- fact5 0     = 1 
-- fact5 (n+1) = (n+1) * fact5 n

-- Using library functions
fact5 :: Int -> Int  
fact5 n = product [1..n]

----- From Section 3.4 to introduce forward and tail recursion 
  
-- Using tail recursive auxilliary function factIter with extra arg
-- Should not allow infinite recursion for negatives
fact6 :: Int -> Int 
fact6 n = factIter n 1 

factIter :: Int -> Int -> Int 
factIter 0 r         = r 
factIter n r | n > 0 = factIter (n-1) (n*r) 

----- Playing around a bit more with more advanced functions

-- Using unbounded Integer type
fact7 :: Integer -> Integer
fact7 n
  | n == 0 =  1 
  | n >= 1 =  n * fact7 (n-1)

-- Using Maybe (as an instance of Functor)
-- Java 8+ Optional is similqr to Haskell's Maybe
-- Java 8+ map functions on Streams are similar to fmap
fact8 :: Int -> Maybe Int
fact8 n | n == 0    = Just 1
fact8 n | n > 0     = fmap (*n) (fact8 (n-1))
fact8 n | otherwise = Nothing

-- Like fact5 except Integer input, Double output
-- [1..n] is a list of Integer, product [1..n] is an Integer
-- fromInteger converts to Double
fact9 :: Integer -> Double
fact9 n = fromInteger (product [1..n])

-- Using foldl -- essentially same as fact5 except unbounded
-- Java 8+ reduce operations on Streams are similar to foldl/foldr
fact10 :: Integer -> Integer
fact10 n = foldl (*) 1 [1..n]

-- Using fold
-- Java 8+ reduce operations on Streams are similar to foldl/foldr
fact11 :: Integer -> Integer
fact11 n = foldr (*) 1 [1..n]

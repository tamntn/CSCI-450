{- Testing Exercises 1-8 from Chapter 2, Programming in Haskell
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-13:  Adapt for current exercises and Haskell 2010

TODO:
-   Needs to be cleaned up and checked more carefully

-}

module Chap02aTest
where

import Prelude hiding (catch)
import Control.Exception

import Homework1  -- this module must export functions tested

passed :: Bool -> String
passed True  = "Passed"
passed False = "FAILED"

-- For testing minf
testf0 :: Int -> Int
testf0 n | n <= 1000 = 0
testf0 n             = 1

-- For testing minf
testf1 :: Int -> Int
testf1 n | n > 1000  = 0
testf1 n             = 1

-- For testing minf
testf2 :: Int -> Int
testf2 n             = 0

-- For testing minf
testf3 :: Int -> Int
testf3 n             = 1


main =
  do
    putStrLn("BEGIN: Chapter 2, part a, exercise testing!")

-- #1  Develop a Haskell function sumsqbig that takes three numbers
--     as arguments and returns the sum of the squares of the two
--     larger numbers. That is sumsqbig 2.0 1.0 3.0 yields 13.

-- I give three possibilities

    putStrLn  "\nTesting sumsqbig1"
    putStrLn ("sumsqbig1 1 2 3 == 13:  " ++
              passed (sumsqbig1 1 2 3 == 13))
    putStrLn ("sumsqbig1 1 3 2 == 13:  " ++
              passed (sumsqbig1 1 3 2 == 13))
    putStrLn ("sumsqbig1 3 1 2 == 13:  " ++
              passed (sumsqbig1 3 1 2 == 13))
    putStrLn ("sumsqbig1 3 2 1 == 13:  " ++
              passed (sumsqbig1 3 2 1 == 13))
    putStrLn ("sumsqbig1 2 1 3 == 13:  " ++
              passed (sumsqbig1 2 1 3 == 13))
    putStrLn ("sumsqbig1 2 3 1 == 13:  " ++
              passed (sumsqbig1 2 3 1 == 13))
    putStrLn ("sumsqbig1 2 2 3 == 13:  " ++
              passed (sumsqbig1 2 2 3 == 13))
    putStrLn ("sumsqbig1 2 3 2 == 13:  " ++
              passed (sumsqbig1 2 2 3 == 13))
    putStrLn ("sumsqbig1 2 3 3 == 18:  " ++
              passed (sumsqbig1 2 3 3 == 18))
    putStrLn ("sumsqbig1 3 2 3 == 18:  " ++
              passed (sumsqbig1 2 3 3 == 18))

    putStrLn  "\nTesting sumsqbig2"
    putStrLn ("sumsqbig2 1 2 3 == 13:  " ++
              passed (sumsqbig2 1 2 3 == 13))
    putStrLn ("sumsqbig2 1 3 2 == 13:  " ++
              passed (sumsqbig2 1 3 2 == 13))
    putStrLn ("sumsqbig2 3 1 2 == 13:  " ++
              passed (sumsqbig2 3 1 2 == 13))
    putStrLn ("sumsqbig2 3 2 1 == 13:  " ++
              passed (sumsqbig2 3 2 1 == 13))
    putStrLn ("sumsqbig2 2 1 3 == 13:  " ++
              passed (sumsqbig2 2 1 3 == 13))
    putStrLn ("sumsqbig2 2 3 1 == 13:  " ++
              passed (sumsqbig2 2 3 1 == 13))
    putStrLn ("sumsqbig2 2 2 3 == 13:  " ++
              passed (sumsqbig2 2 2 3 == 13))
    putStrLn ("sumsqbig2 2 3 2 == 13:  " ++
              passed (sumsqbig2 2 2 3 == 13))
    putStrLn ("sumsqbig2 2 3 3 == 18:  " ++
              passed (sumsqbig2 2 3 3 == 18))
    putStrLn ("sumsqbig2 3 2 3 == 18:  " ++
              passed (sumsqbig2 2 3 3 == 18))

    putStrLn  "\nTesting sumsqbig13"
    putStrLn ("sumsqbig3 1 2 3 == 13:  " ++
              passed (sumsqbig3 1 2 3 == 13))
    putStrLn ("sumsqbig3 1 3 2 == 13:  " ++
              passed (sumsqbig3 1 3 2 == 13))
    putStrLn ("sumsqbig3 3 1 2 == 13:  " ++
              passed (sumsqbig3 3 1 2 == 13))
    putStrLn ("sumsqbig3 3 2 1 == 13:  " ++
              passed (sumsqbig3 3 2 1 == 13))
    putStrLn ("sumsqbig3 2 1 3 == 13:  " ++
              passed (sumsqbig3 2 1 3 == 13))
    putStrLn ("sumsqbig3 2 3 1 == 13:  " ++
              passed (sumsqbig3 2 3 1 == 13))
    putStrLn ("sumsqbig3 2 2 3 == 13:  " ++
              passed (sumsqbig3 2 2 3 == 13))
    putStrLn ("sumsqbig3 2 3 2 == 13:  " ++
              passed (sumsqbig3 2 2 3 == 13))
    putStrLn ("sumsqbig3 2 3 3 == 18:  " ++
              passed (sumsqbig3 2 3 3 == 18))
    putStrLn ("sumsqbig3 3 2 3 == 18:  " ++
              passed (sumsqbig3 2 3 3 == 18))

-- NOT ASSIGNED FALL 2017
-- #2  Develop function xor that takes two Booleans and returns the 
--     "exclusive-or" of the two values. An exclusive-or operation 
--     returns True when exactly one of its arguments is True and 
--     returns False otherwise. 

    putStrLn  "\nTesting xor'"
    putStrLn ("xor' True True   == False: " ++
              passed (xor' True  True  == False))
    putStrLn ("xor' True False  == True:  " ++
              passed (xor' True  False == True))
    putStrLn ("xor' False True  == True:  " ++
              passed (xor' False True  == True))
    putStrLn ("xor' False False == False: " ++
              passed (xor' False False == False))

-- #3  Develop a Haskell Boolean function div23n5 such that div23n5 n
--     returns True if and only if n is divisible by 2 or divisible by
--     3 but not divisible by 5. That is, div23n5 6 yields True and
--     div23n5 30 yields False.

    putStrLn  "\nTesting div23n5"
    putStrLn ("div23n5  2 == True:  " ++ passed (div23n5  2 == True))
    putStrLn ("div23n5  3 == True:  " ++ passed (div23n5  3 == True))
    putStrLn ("div23n5  5 == False: " ++ passed (div23n5  5 == False))
    putStrLn ("div23n5  6 == True:  " ++ passed (div23n5  6 == True))
    putStrLn ("div23n5  7 == False: " ++ passed (div23n5  7 == False))
    putStrLn ("div23n5 10 == False: " ++ passed (div23n5 10 == False))
    putStrLn ("div23n5 15 == False: " ++ passed (div23n5 15 == False))
    putStrLn ("div23n5 30 == False: " ++ passed (div23n5 30 == False))
    putStrLn ("div23n5 36 == True:  " ++ passed (div23n5 36 == True))


-- #4 Develop a Haskell function notDiv such that notDiv n d returns
--    True if and only if integer n is not divisible by d. That is,
--    notDiv 10 5 yields False and notDiv 11 5 yields True.

    putStrLn  "\nTesting notDiv"
    putStrLn ("notDiv 2 3 == True:  " ++ passed (notDiv 2 3 == True))
    putStrLn ("notDiv 3 3 == False: " ++ passed (notDiv 3 3 == False))
    putStrLn ("notDiv 4 3 == True:  " ++ passed (notDiv 4 3 == True))
    putStrLn ("notDiv 5 3 == True:  " ++ passed (notDiv 5 3 == True))
    putStrLn ("notDiv 6 3 == False: " ++ passed (notDiv 6 3 == False))


-- #5 Develop a Haskell function mult that takes two *natural numbers*
--    (i.e., nonnegative integers) and returns their product. The
--    function must not use the multiplication (*) or division (div)
--    operators. Hint: Multiplication can be done by repeated
--    addition.

-- I give two alternatives

    putStrLn  "\nTesting mult1"
    putStrLn ("mult1 0 3 == 0:        " ++
              passed (mult1 0 3 == 0))
    putStrLn ("mult1 1 3 == 3:        " ++
              passed (mult1 1 3 == 3))
    putStrLn ("mult1 2 3 == 6:        " ++
              passed (mult1 2 3 == 6))
    putStrLn ("mult1 2 (-3) == -6:    " ++
              passed (mult1 2 (-3) == -6))

    putStrLn "Next test should cause an error!"
    putStr "mult1 (-1) 3 ==  -3:  "
    putStrLn (passed (mult1 (-1) 3 == -3))
        `catch` (\(ErrorCall msg)
                 -> putStrLn $ "[ERROR] " ++ msg)
 
    putStrLn  "\nTesting mult2"
    putStrLn ("mult2 0 3 == 0:        " ++
              passed (mult2 0 3 == 0))
    putStrLn ("mult2 1 3 == 3:        " ++
              passed (mult2 1 3 == 3))
    putStrLn ("mult2 2 3 == 6:        " ++
              passed (mult2 2 3 == 6))
    putStrLn ("mult2 2 (-3) == -6:    " ++
              passed (mult2 2 (-3) == -6))
    putStrLn ("mult2 (-1) 3 == -3:    " ++
              passed (mult2 (-1) 3 == -3))
    putStrLn ("mult2 (-2) (-3) == 6:  " ++
              passed (mult2 (-2) (-3) == 6))

-- #6  Develop a Haskell function addTax that takes two Double values
--     such that addTax c p returns c with a sales tax of p percent
--     added. For example, addTax 2.0 9.0 returns 2.18.

-- Does not currently test possible negative values for args c and p

    putStrLn  "\nTesting addTax and subTax"
    putStrLn ("addTax 10 9 == 10.90:      " ++
              passed (addTax 10 9 == 10.90))
    putStrLn ("subTax 10.90 9 == 10:      " ++
              passed (subTax 10.90 9 == 10))

    putStrLn ("addTax 100.0 7.5 == 107.5: " ++
              passed (addTax 100.0 7.5 == 107.5))
    putStrLn ("subTax 107.5 7.5 == 100.0: " ++
              passed (subTax 107.5 7.5 == 100.0))


-- 7.  The time of day can be represented by a tuple (hours,minutes,m)
--     where m indicates either "AM" or "PM". Develop a Boolean
--     Haskell function comesBefore that takes two time-of-day tuples
--     and determines whether the first is an earlier time than the
--     second.

--  Tricky: (12,0,"AM") is earliest time. (12,0,"PM") earliest PM

    putStrLn  "\nTesting comesBefore"
    putStrLn ("comesBefore (12,0,\"AM\") (12,0,\"AM\"):  " ++
             passed (comesBefore (12,0,"AM") (12,0,"AM")  == False))
    putStrLn ("comesBefore (12,0,\"AM\") (9,15,\"AM\"):  " ++
             passed (comesBefore (12,0,"AM") (9,15,"AM") == True))
    putStrLn ("comesBefore (12,0,\"AM\") (12,0,\"PM\"):  " ++
             passed (comesBefore (12,0,"AM") (12,0,"PM")  == True))
    putStrLn ("comesBefore (12,0,\"AM\") (11,59,\"PM\"): " ++
             passed (comesBefore (12,0,"AM") (11,59,"PM") == True))

    putStrLn ("comesBefore (12,0,\"PM\") (12,0,\"AM\"):  " ++
             passed (comesBefore (12,0,"PM") (12,0,"AM")  == False))
    putStrLn ("comesBefore (12,0,\"PM\") (9,15,\"AM\"):  " ++
             passed (comesBefore (12,0,"PM") (9,15,"AM")  == False))
    putStrLn ("comesBefore (12,0,\"PM\") (12,0,\"PM\"):  " ++
             passed (comesBefore (12,0,"PM") (12,0,"PM")  == False))
    putStrLn ("comesBefore (12,0,\"PM\") (11,59,\"PM\"): " ++
             passed (comesBefore (12,0,"PM") (11,59,"PM") == True))
 
    putStrLn ("comesBefore (9,15,\"AM\") (12,0,\"AM\"):  " ++
             passed (comesBefore (9,15,"AM") (12,0,"AM")  == False))
    putStrLn ("comesBefore (9,15,\"AM\") (9,15,\"AM\"):  " ++
             passed (comesBefore (9,15,"AM") (9,15,"AM") == False))
    putStrLn ("comesBefore (9,15,\"AM\") (12,0\"PM\"):   " ++
             passed (comesBefore (9,15,"AM") (12,0,"PM")  == True))
    putStrLn ("comesBefore (9,15,\"AM\") (11,59,\"PM\"): " ++
             passed (comesBefore (9,15,"AM") (11,59,"PM") == True))

    putStrLn ("comesBefore (9,15,\"AM\") (8,59,\"AM\"):  " ++
             passed (comesBefore (9,15,"AM") (8,59,"AM")  == False))
    putStrLn ("comesBefore (9,15,\"AM\") (9,14,\"AM\"):  " ++
             passed (comesBefore (9,15,"AM") (9,14,"AM")  == False))
    putStrLn ("comesBefore (9,15,\"AM\") (9,16,\"AM\"):  " ++
             passed (comesBefore (9,15,"AM") (9,16,"AM")  == True))
    putStrLn ("comesBefore (9,15,\"AM\") (9,15,\"PM\"):  " ++
             passed (comesBefore (9,15,"AM") (9,15,"PM")  == True))

    putStrLn ("comesBefore (9,15,\"PM\") (12,0,\"AM\"):  " ++
             passed (comesBefore (9,15,"PM") (12,0,"AM")  == False))
    putStrLn ("comesBefore (9,15,\"PM\") (9,15,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (9,15,"PM") == False))
    putStrLn ("comesBefore (9,15,\"AM\") (12,0,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (12,0,"PM")  == False))
    putStrLn ("comesBefore (9,15,\"AM\") (11,59,\"PM\"): " ++
             passed (comesBefore (9,15,"PM") (11,59,"PM") == True))

    putStrLn ("comesBefore (9,15,\"PM\") (9,15,\"AM\"):  " ++
             passed (comesBefore (9,15,"PM") (9,15,"AM") == False))
    putStrLn ("comesBefore (9,15,\"PM\") (8,59,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (8,59,"PM")  == False))
    putStrLn ("comesBefore (9,15,\"PM\") (9,14,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (9,14,"PM")  == False))
    putStrLn ("comesBefore (9,15,\"PM\") (9,16,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (9,16,"PM")  == True))

    putStrLn "\nNext 6 tests should cause errors"
    putStrLn ("comesBefore (9,15,\"XX\") (9,16,\"PM\"):  " ++
             passed (comesBefore (9,15,"XX") (9,16,"PM")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)
    putStrLn ("comesBefore (9,15,\"PM\") (9,16,\"XX\"):  " ++
             passed (comesBefore (9,15,"PM") (9,16,"XX")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)
    putStrLn ("comesBefore (13,15,\"PM\") (9,16,\"PM\"): " ++
             passed (comesBefore (13,15,"PM") (9,16,"PM")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)
    putStrLn ("comesBefore (9,15,\"PM\") (0,16,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (0,16,"PM")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)
    putStrLn ("comesBefore (9,60,\"PM\") (9,16,\"PM\"):  " ++
             passed (comesBefore (9,60,"PM") (9,16,"PM")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)
    putStrLn ("comesBefore (9,15,\"PM\") (9,60,\"PM\"):  " ++
             passed (comesBefore (9,15,"PM") (9,60,"PM")  == True))
             `catch` (\(ErrorCall msg)
                      -> putStrLn $ "[ERROR] " ++ msg)

-- #8. Develop a Haskell function 
--
--         minf :: (Int -> Int) -> Int
--		
--    such that `minf g` returns the smallest integer `m` such that 
--    0 <= m <= 10000000 and g m == 0 (if such an integer exists).

    putStrLn  "\nTesting minf"
    putStrLn ("minf testf0 == 0:    " ++ passed (minf testf0 == 0))
    putStrLn ("minf testf1 == 1001: " ++ passed (minf testf1 == 1001))
    putStrLn ("minf testf2 == 0:    " ++ passed (minf testf2 == 0))

    putStrLn "Next test should run a long time and then cause error!"
    putStr   "minf testf3 error:    "
    putStrLn (passed (minf testf3 == 0))
        `catch` (\(ErrorCall msg)
                 -> putStrLn $ "[ERROR] " ++ msg)

    putStrLn("END: Chapter 2, part a, exercise testing!")

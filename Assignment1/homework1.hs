-- CSCI 450
-- Homework 1
-- Tam Nguyen

module Homework1(
    sumsqbig, div23n5, notDiv, mult, addTax, subTax, comesBefore
)
where

-- 1.
-- Develop a Haskell function sumsqbig that takes three numbers as arguments and 
-- returns the sum of the squares of the two larger numbers.
    sumsqbig :: Double -> Double -> Double -> Double
    sumsqbig x y z = 
                    if x > y && y > z then
                        x * x + y * y
                    else if x > y && z > y then
                        x * x + z * z
                    else
                        y * y + z * z

-- 2. OMITTED

-- 3.
-- Develop a Haskell Boolean function div23n5 such that div23n5 n returns True if and only if n is divisible by 2 or divisible by 3 but not divisible by 5. 
-- That is, div23n5 6 yields True and div23n5 30 yields False.
    div23n5 :: Int -> Bool
    div23n5 n = 
                if rem n 2 == 0 && rem n 3 == 0 then
                    if rem n 5 == 0 then
                        False
                    else
                        True
                else
                    False

-- 4.
-- Develop a Haskell function notDiv such that notDiv n d returns True if and only if integer n is not divisible by d. 
-- That is, notDiv 10 5 yields False and notDiv 11 5 yields True.
    notDiv :: Int -> Int -> Bool
    notDiv n d = 
                if rem n d == 0 then
                    False
                else
                    True

-- 5.
-- Develop a Haskell function mult that takes two natural numbers (i.e., nonnegative integers) and returns their product. 
-- The function must not use the multiplication (*) or division (div) operators.
-- Hint: Multiplication can be done by repeated addition.
    mult :: Int -> Int -> Int
    mult a b
      | b == 0    = 0
      | otherwise = sum(replicate b a)

-- 6.
-- Develop a Haskell function addTax that takes two Double values such that addTax c p returns c with a sales tax of p percent added. 
-- For example, addTax 2.0 9.0 returns 2.18.
-- Also develop a function subTax that is the inverse of addTax. That is, subTax (addTax c p) p yields c.
    addTax :: Double -> Double -> Double
    addTax c p = c * (100+p)/100

    subTax :: Double -> Double -> Double
    subTax c p = c * 100/(100+p)

-- 7.
-- The time of day can be represented by a tuple (hours,minutes,m) where m indicates either "AM" or "PM". 
-- Develop a Boolean Haskell function comesBefore that takes two time-of-day tuples and determines whether the first is an earlier time than the second.
    first (a, b, c) = a

    second (a, b, c) = b

    third :: (a, b, c) -> c
    third (a, b, c) = c

    comesBefore :: (Int, Int, String) -> (Int, Int, String) -> Bool
    comesBefore a b
     | third(a) == "AM" && third(b) == "PM" = True
     | third(a) == third(b) && first(a) < first(b) = True
     | third(a) == third(b) && first(a) == first(b) && second(a) < second(b) = True
     | otherwise = False

 {- comesBefore :: (Int, Int, String) -> (Int, Int, String) -> Bool
    comesBefore a b =
        if third(a) == "AM" && third(b) == "PM" then
            True
        else if third(a) == "PM" && third(b) == "AM" then
            False
        else
            if first(a) < first(b) then
                True
            else if first(a) > first(b) then
                False
            else
                if second(a) < second(b) then
                    True
                else
                    False
-}

-- 8.
-- Develop a Haskell function
-- minf :: (Int -> Int) -> Int
-- such that minf g returns the smallest integer m such that 0 <= m <= 10000000 and g m == 0 (if such an integer exists).

    --minf :: (Int -> Int) -> Int

    g :: Int -> Int
    g n | n > 1000 = 0
    g n            = n
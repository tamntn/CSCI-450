{-  Square Root Module
    Section 2.5 of Basic Haskell Functional Programming notes
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

    2016-07-??: Based on SICP 1.1.7 & my Scala, Elixir, Lua versions
    2017-02-03: Added comments
-}

module Sqrt
    (sqrt')
where

import Debug.Trace

sqrt' :: Double -> Double
sqrt' x | x >= 0 = sqrt_iter 1 x

sqrt_iter :: Double -> Double -> Double
sqrt_iter guess x
    | good_enough guess x = guess
    | otherwise           = sqrt_iter (improve guess x) x

good_enough :: Double -> Double -> Bool
good_enough guess x = abs (square guess - x) < 0.001

square :: Double -> Double
square x = x * x

average :: Double -> Double -> Double
average x y = (x + y) / 2

improve :: Double -> Double -> Double
improve guess x = average guess (x/guess)


{-  Rational Arithmetic Deferred GCD Data Representation
    Section 2.6 of Basic Haskell Functional Programming
    H. Conrad Cunningham

    This module encapsulates one possible data representation for
    rational numbers: using two relatively prime integers.

1234567890123456789012345678901234567890123456789012345678901234567890

    2016-07-??: Based on SICP 2.1 & my earler Haskell, Lua versions
    2017-02-03: Added comments

-}

module RationalDeferGCD
  (Rat, makeRat, numer, denom, showRat)
where

-- Core

type Rat = (Int,Int)

makeRat :: Int -> Int -> Rat
makeRat x 0 = error ( "Cannot construct a rational number "
                       ++ showRat (x,0) ++ "\n" ) 
makeRat 0 y = (0,1) 
makeRat x y = (x,y)

numer, denom :: Rat -> Int
numer (x,y) = x' `div` d
    where x' = (signum' y) * x 
          y' = abs' y 
          d  = gcd' x' y'

denom (x,y) = y' `div` d
    where x' = (signum' y) * x
          y' = abs' y 
          d  = gcd' x' y'

showRat :: Rat -> String
showRat x = show (numer x) ++ "/" ++ show (denom x)

--- Utilities

signum' :: Int -> Int
signum' n | n == 0 =  0 
          | n > 0  =  1 
          | n < 0  = -1
            
abs' :: Int -> Int 
abs' n | n >= 0 =  n 
       | n <  0 = -n 
 
gcd' :: Int -> Int -> Int 
gcd' x y = gcd'' (abs' x) (abs' y) 
         where gcd'' x 0 = x 
               gcd'' x y = gcd'' y (x `rem` y)


{-  Rational Arithmetic Module (outer)
    Section 2.6 of Basic Haskell Functional Programming
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

    2016-07-??: Based on SICP 2.1 & my earler Haskell, Lua versions
    2017-02-03: Added comments

-}

module Rational 
  ( Rat, makeRat, numer, denom, showRat,
    negRat, addRat, subRat, mulRat, divRat, eqRat )
where

-- Select needed data representation module
import RationalCore
-- import RationalDeferGCD

-- First-order Operations

negRat :: Rat -> Rat 
negRat x = makeRat (- numer x) (denom x)

addRat, subRat, mulRat, divRat :: Rat -> Rat -> Rat 
addRat x y = makeRat (numer x * denom y + numer y * denom x)
                     (denom x * denom y) 
subRat x y = makeRat (numer x * denom y - numer y * denom x)
                     (denom x * denom y) 
mulRat x y = makeRat (numer x * numer y) (denom x * denom y) 
divRat x y = makeRat (numer x * denom y) (denom x * numer y) 

eqRat :: Rat -> Rat -> Bool 
eqRat x y = (numer x) * (denom y) == (numer y) * (denom x)      

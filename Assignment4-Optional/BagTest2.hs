{- CSci 450/503, Asignment #4, Fall 2017
   Blackbox Testing for multiple Bag modules
   Exercise 14 from Chapter 4, Functional Programming Using Haskell
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-10-16: Original Haskell version using unordered lists
2017-10-17: Separated testing module from Bag module
2017-10-24: Improved tests, made independent of representation
2017-10-25: Typo in test message corrected, format cleaned

This testing module seeks to test "any" Bag module that meets the
problem specification. It assumes that the element type is in class
Ord (i.e., they are totally ordered, allowing <, <=, >, >=, ==, and
/=).

The testing here may have some redundant tests and leave out a few
tests that should be done.

(This module probably should be augmented by tests that check any
problematic situation with a specific representation.)

-}

module BagTest
where

import Data.List ( sort, delete, nub )

import Bag       -- unordered list of elements [a] rep for Bag a
-- import BagTuple  -- ordered list of tuples (a,Int) rep for Bag a

pass :: Bool -> String
pass True  = "SUCCESS"
pass False = "FAIlURE"

main = do

    {- I defined lists and bags with the "same" values for testing.
       I take advantage of these relationships in my testing.

       Most likely, I should have other test data.
    -}

    -- Data used in tests
    -- s0 and b0 help test for empty bags
    let s0 = [] :: [Int]  -- force to element type Int for these
    putStr "List s0:  "
    putStrLn $ show s0 
    let b0 = listToBag s0
    putStrLn "b0 = listToBag s0"

    -- s1 and b1 help test for a nonempty bag with occurrences > 1
    let s1 = [2,4,2,2]
    putStr "List s1:  "
    putStrLn $ show s1
    let b1 = listToBag s1
    putStrLn "b1 = listToBag s1"

    -- s2 is same as s1 except order shuffled
    let s2 = [2,2,2,4]
    putStr "List s2:  "
    putStrLn $ show s2
    let b2 = listToBag s2
    putStrLn "b2 = listToBag s2"

    -- s3 is s1 with duplicates removed
    let s3 = [2,4]  -- could do "s3 = nub s1"
    putStr "List s3:  "
    putStrLn $ show s3
    let b3 = listToBag s3
    putStrLn "b3 = listToBag s3"

    -- s4 has intersection with s1 but different # occurrences
    let s4 = [2,2,2,2]
    putStr "List s4:  "
    putStrLn $ show s4
    let b4 = listToBag s4
    putStrLn "b4 = listToBag s4"

    -- s5 
    let s5 = [8]
    putStr "List s5:  "
    putStrLn $ show s5
    let b5 = listToBag s5
    putStrLn "b5 = listToBag s5"

    {- There is no guarantee for all representations that
           bagToList (listToBag xs) == xs
       but the following should hold
           sort (bagToList (listToBag xs)) == sort xs

       I test listToBag and bagToList early because they are useful
       in testing other functions. The tests of listToBag are
       indirect in terms of bagToList.
    -}

    -- Test newBag, listToBag, and bagToList
    putStrLn "\nTest newBag, bagToList, and listToBag"
    -- check emtpy bag and newBag
    putStr "bagToList b0 == []:                                 "
    putStrLn $ pass $ bagToList b0 == [] 
    putStr "bagToList newBag == s0:                             "
    putStrLn $ pass $ bagToList newBag == s0
    -- check nonempty bags (built above with listToBag)
    putStr "sort (bagToList b1) == sort s1:                     "
    putStrLn $ pass $ sort (bagToList b1) == sort s1
    putStr "sort (bagToList b2) == sort s2:                     "
    putStrLn $ pass $ sort (bagToList b2) == sort s2
    putStr "sort (bagToList b3) == sort s3:                     "
    putStrLn $ pass $ sort (bagToList b3) == sort s3
    putStr "sort (bagToList b4) == sort s4:                     "
    putStrLn $ pass $ sort (bagToList b4) == sort s4
    putStr "bagToList b5 == s5                                  "
    putStrLn $ pass $ bagToList b5 == s5
    
    -- Test isEmpty
    putStrLn "\nTest isEmpty"
    -- check empty bag
    putStr "isEmpty b0 == True:                                 "
    putStrLn $ pass $ isEmpty b0 == True
    -- check nonempty bag
    putStr "isEmpty b1 == False:                                "
    putStrLn $ pass $ isEmpty b1 == False
    putStr "isEmpty b5 == False:                                "
    putStrLn $ pass $ isEmpty b5 == False

    -- Test isElem
    putStrLn "\nTest isEmpty"
    -- check empty bag
    putStr "isElem 1 b0 == False:                               " 
    putStrLn $ pass $ isElem 1 b0 == False
    -- check nonempty bags with element
    putStr "isElem 2 b1 == True:                                "
    putStrLn $ pass $ isElem 2 b1 == True
    putStr "isElem 4 b1 == True:                                "
    putStrLn $ pass $ isElem 4 b1 == True
    -- check nonempty bags without element
    putStr "isElem 8 b1 == False:                               "
    putStrLn $ pass $ isElem 8 b1 == False

    -- Test size
    putStrLn "\nTest size"
    -- check empty bag
    putStr "size b0 == 0:                                       "
    putStrLn $ pass $ size b0 == 0
    -- check nonempty bag
    putStr "size b1 == 4:                                       "
    putStrLn $ pass $ size b1 == 4  -- length t1

    -- Test occursBag
    putStrLn "\nTest occursBag"
    -- check empty bag
    putStr "occursBag 2 b0 == 0:                                "
    putStrLn $ pass $ occursBag 2 b0 == 0
    -- check nonempty bag with element
    putStr "occursBag 2 b1 == 3:                                "
    putStrLn $ pass $ occursBag 2 b1 == 3  -- filter (==2) t1
    putStr "occursBag 4 b1 == 1:                                "
    putStrLn $ pass $ occursBag 4 b1 == 1  -- filter (==4) t
    -- check nonempty bag without element
    putStr "occursBag 8 b1 == 0:                                "
    putStrLn $ pass $ occursBag 8 b1 == 0  -- filter (==8) t1


    -- Test insertBag
    putStrLn "\nTest insertBag"
    -- check empty bag
    putStr "bagToList (insertBag 8 b0) == [8]:                  "
    putStrLn $ pass $ bagToList (insertBag 8 b0) == [8]
    -- check nonempty bag, adding existing element
    putStr "sort (bagToList (insertBag 4 b1)) == sort (4:s1):   "
    putStrLn $ pass $
               sort (bagToList (insertBag 4 b1)) == sort (4:s1)
    -- check nonempty bag, adding new element
    putStr "sort (bagToList (insertBag 8 b1)) == sort (8:s1):   "
    putStrLn $ pass $
               sort (bagToList (insertBag 8 b1)) == sort (8:s1)

    -- Test deleteBag
    -- Spec vague on what should happen when element not in bag
    -- Here we assume unmodified bag returned
    putStrLn "\nTest deleteBag"
    -- check empty
    putStr "bagToList (deleteBag 2 b0) == []:                        "
    putStrLn $ pass $ bagToList (deleteBag 2 b0) == []
    -- check nonempty bag, removing existing element
    putStr "sort (bagToList (deleteBag 2 b1)) == sort (delete 2 s1): "
    putStrLn $ pass $
               sort (bagToList (deleteBag 2 b1)) == sort (delete 2 s1)
    -- check nonempty, deleting element not in bag
    putStr "sort (bagToList (deleteBag 8 b1) == sort (delete 8 s1):  "
    putStrLn $ pass $
              sort (bagToList (deleteBag 8 b1)) == sort s1
   
    -- Test eqBag
    -- check equality in both directions, must be symmetric
    -- Can use eqBag in later testing
    putStrLn "\nTest eqBag"
    -- check empty bags for both
    putStr "eqBag b0 b0 == True:                                 "
    putStrLn $ pass $ eqBag b0 b0 == True
    -- check with one nonempty bag, one empty
    putStr "eqBag b0 b1 == False:                                "
    putStrLn $ pass $ eqBag b0 b1 == False
    putStr "eqBag b1 b0 == False:                                "
    putStrLn $ pass $ eqBag b1 b0 == False
    -- check nonempty bags equals self
    putStr "eqBag b1 b1 == True:                                 "
    putStrLn $ pass $ eqBag b1 b1 == True
    -- check nonempty bags, same content but separate
    putStr "eqBag b1 b2 == True:                                 "
    putStrLn $ pass $ eqBag b1 b2 == True
    putStr "eqBag b2 b1 == True:                                 "
    putStrLn $ pass $ eqBag b2 b1 == True
    -- check with two nonempty bags, some common elements
    putStr "eqBag b2 b3 == False:                                "
    putStrLn $ pass $ eqBag b2 b3 == False
    putStr "eqBag b3 b2 == False:                                "
    putStrLn $ pass $ eqBag b3 b2 == False
    -- check with two nonempty bags, no common elements
    putStr "eqBag b2 b5 == False:                                "
    putStrLn $ pass $ eqBag b2 b5 == False
    putStr "eqBag b5 b2 == False:                                "
    putStrLn $ pass $ eqBag b5 b2 == False

    -- Test unionBag
    putStrLn "\nTest unionBag"
    -- check with both empty bags
    putStr "eqBag b0 (unionBag b0 b0) = True:                    "
    putStrLn $ pass $ eqBag b0 (unionBag b0 b0) == True
    -- check with one nonempty, one empty bag
    putStr "eqBag b2 (unionBag b2 b0) == True:                   "
    putStrLn $ pass $ eqBag b2 (unionBag b2 b0) == True
    putStr "eqBag b2 (unionBag b0 b2) == True:                   "
    putStrLn $ pass $ eqBag b2 (unionBag b0 b2) == True
    -- check with two nonempty bags, same content but separate
    putStr "eqBag b1 (unionBag b1 b2)) == True:                  "
    putStrLn $ pass $ eqBag b1 (unionBag b1 b2) == True
    putStr "eqBag b1 (unionBag b2 b1) == True:                   " 
    putStrLn $ pass $ eqBag b1 (unionBag b2 b1) == True
    -- check with two nonempty bags, some common elements
    putStr "eqBag b2 (unionBag b2 b3) == True:                   "
    putStrLn $ pass $ eqBag b2 (unionBag b2 b3) == True
    putStr "eqBag b2 (unionBag b3 b2) == True:                   "
    putStrLn $ pass $ eqBag b2 (unionBag b3 b2) == True
    putStr "eqBag (unionBag b2 b4) (listToBag (4:s4)) == True:   "
    putStrLn $ pass $
               eqBag (unionBag b2 b4) (listToBag (4:s4)) == True
    putStr "eqBag (unionBag b4 b2) (listToBag (4:s4)) == True:   "
    putStrLn $ pass $
               eqBag (unionBag b4 b2) (listToBag (4:s4)) == True
    -- check with two nonempty bags, no common elements
    putStr "eqBag (unionBag b2 b5) (listToBag (8:s2)) == True:   "
    putStrLn $ pass $
               eqBag (unionBag b2 b5) (listToBag (8:s2)) == True
    putStr "eqBag (unionBag b5 b2) (listToBag (8:b2)) == True:   "
    putStrLn $ pass $
               eqBag (unionBag b5 b2) (listToBag (8:s2)) == True

    -- Test intersectBag
    putStrLn "\nTest intersectBag"
    -- check with both empty bags
    putStr "eqBag b0 (intersectBag b0 b0) == True:               "
    putStrLn $ pass $ eqBag b0 (intersectBag b0 b0) == True
    -- check with one nonempty, one empty bag
    putStr "eqBag b0 (intersectBag b1 b0) == True:               "
    putStrLn $ pass $ eqBag b0 (intersectBag b1 b0) == True
    putStr "eqBag b0 (intersectBag b0 b1) == True:               "
    putStrLn $ pass $ eqBag b0 (intersectBag b0 b1) == True
    -- check with two nonempty bags, same content but separate
    putStr "eqBag b1 (intersectBag b1 b2) == True:               "
    putStrLn $ pass $ eqBag b1 (intersectBag b1 b2) == True
    putStr "eqBag b1 (intersectBag b2 b1) == True:               "
    putStrLn $ pass $ eqBag b1 (intersectBag b2 b1) == True
    -- check two nonempty bags, some common elements
    putStr "eqBag b3 (intersectBag b1 b3) == True:               "
    putStrLn $ pass $ eqBag b3 (intersectBag b1 b3) == True
    putStr "eqBag b3 (intersectBag b3 b1) == True:               "
    putStrLn $ pass $ eqBag b3 (intersectBag b3 b1) == True
    putStr "eqBag (listToBag [2,2,2]) (intersectBag b1 b4) == True:  "
    putStrLn $ pass $
               eqBag (listToBag [2,2,2]) (intersectBag b1 b4) == True
    putStr "eqBag (listToBag [2,2,2]) (intersectBag b4 b1) == True:  "
    putStrLn $ pass $
               eqBag (listToBag [2,2,2]) (intersectBag b4 b1) == True
    -- check two nonempty bags, no common elements
    putStr "eqBag b0 (intersectBag b5 b2) == True:               "
    putStrLn $ pass $ eqBag b0 (intersectBag b5 b2) == True
    putStr "eqBag b0 (intersectBag b2 b5) == True:               "
    putStrLn $ pass $ eqBag b0 (intersectBag b2 b5) == True

    -- Test sumBag
    putStrLn "\nTest sumBag"
    -- check with both empty bags
    putStr "eqBag b0 (sumBag b0 b0) = True:                      "
    putStrLn $ pass $ eqBag b0 (sumBag b0 b0) == True
    -- check with one nonempty, one empty
    putStr "eqBag b2 (sumBag b2 b0) == True:                     "
    putStrLn $ pass $ eqBag b2 (sumBag b2 b0) == True
    putStr "eqBag b2 (sumBag b0 b2) == True:                     "
    putStrLn $ pass $ eqBag b2 (sumBag b0 b2) == True
    -- check with two nonempty bags, same content but separate
    putStr "eqBag (listToBag (s1++s2)) (sumBag b1 b2)) == True:  "
    putStrLn $ pass $
               eqBag (listToBag (s1++s2)) (sumBag b1 b2) == True
    putStr "eqBag (listToBag (s1++s2)) (sumBag b2 b1)) == True:  "
    putStrLn $ pass $
               eqBag (listToBag (s1++s2)) (sumBag b2 b1) == True
    -- check two nonempty bags, some common elements
    putStr "eqBag (listToBag (s1++s3)) (sumBag b1 b3) == True:   "
    putStrLn $ pass $
               eqBag (listToBag (s1++s3)) (sumBag b1 b3) == True
    putStr "eqBag (listToBag (s1++s3)) (sumBag b3 b1) == True:   "
    putStrLn $ pass $
               eqBag (listToBag (s1++s3)) (sumBag b3 b1) == True
    putStr "eqBag (listToBag (s1++s4) (sumBag b1 b4) == True:    "
    putStrLn $ pass $
               eqBag (listToBag (s1++s4)) (sumBag b1 b4) == True
    putStr "eqBag (listToBag (s1++s4)) (sumBag b4 b1) == True:   " 
    putStrLn $ pass $
               eqBag (listToBag (s1++s4)) (sumBag b4 b1) == True
    -- check two nonempty bags, no common elements
    putStr "eqBag (listToBag (s2++s5)) (sumBag b5 b2) == True:   "
    putStrLn $ pass $
               eqBag (listToBag (s2++s5)) (sumBag b5 b2) == True
    putStr "eqBag (listToBag (s2++s5)) (sumBag b2 b5) == True:   "
    putStrLn $ pass $
              eqBag (listToBag (s2++s5)) (sumBag b2 b5) == True

    -- Test diffBag
    putStrLn "\nTest diffBag"
    -- check with both empty bags
    putStr "eqBag b0 (diffBag b0 b0) = True:                     "
    putStrLn $ pass $ eqBag b0 (diffBag b0 b0) == True
    -- check with first bag empty, second nonempty
    putStr "eqBag b0 (diffBag b0 b1) = True:                     "
    putStrLn $ pass $ eqBag b0 (diffBag b0 b1) == True
    -- check with first bag nonempty, second empty
    putStr "eqBag b1 (diffBag b1 b0) = True:                     "
    putStrLn $ pass $ eqBag b1 (diffBag b1 b0) == True
    -- check with both nonempty, but same contents
    putStr "eqBag b0 (diffBag b1 b2)) == True:                   "
    putStrLn $ pass $ eqBag b0 (diffBag b1 b2) == True
    putStr "eqBag b0 (diffBag b2 b1)) == True:                   "
    putStrLn $ pass $ eqBag b0 (diffBag b2 b1) == True
    -- check with both nonempty, one subbag of other
    putStr "eqBag (listToBag [2,2]) (diffBag b1 b3) == True:     "
    putStrLn $ pass $ eqBag (listToBag [2,2]) (diffBag b1 b3) == True
    putStr "eqBag b0 (diffBag b3 b1) == True:                    "
    putStrLn $ pass $ eqBag b0 (diffBag b3 b1) == True
    -- check with both nonempty, some common elements
    putStr "eqBag (listToBag [4]) (diffBag b1 b4) == True:       "
    putStrLn $ pass $ eqBag (listToBag [4]) (diffBag b1 b4) == True
    putStr "eqBag (listToBag [2]) (diffBag b4 b1) == True:       "
    putStrLn $ pass $ eqBag (listToBag [2]) (diffBag b4 b1) == True
    -- check two nonempty bags, no common elements
    putStr "eqBag b2 (diffBag b2 b5) == True:                    "
    putStrLn $ pass $ eqBag b2 (diffBag b2 b5) == True
    putStr "eqBag b5 (diffBag b5 b2) == True:                    "
    putStrLn $ pass $ eqBag b5 (diffBag b5 b2) == True

    -- Test subBag
    putStrLn "\nTest subBag"
    -- check with both empty bags
    putStr "subBag b0 b0 = True:                                 "
    putStrLn $ pass $ subBag b0 b0 == True
    -- check with first bag empty, second nonempty
    putStr "subBag b0 b1 = True:                                 "
    putStrLn $ pass $ subBag b0 b1 == True
    -- check with first bag nonempty, second empty
    putStr "subBag b1 b0 = False:                                "
    putStrLn $ pass $ subBag b1 b0 == False
    -- check with both nonempty, but same contents
    putStr "subBag b1 b2 == True:                                "
    putStrLn $ pass $ subBag b1 b2 == True
    putStr "subBag b2 b1 == True:                                "
    putStrLn $ pass $ subBag b2 b1 == True
    -- check with both nonempty, one subbag of other
    putStr "subBag b1 b3 == False:                               "
    putStrLn $ pass $ subBag b1 b3 == False
    putStr "subBag b3 b1 == True:                                "
    putStrLn $ pass $ subBag b3 b1 == True
    -- check with both nonempty, some common elements
    putStr "subBag b1 b4 == False:                               "
    putStrLn $ pass $ subBag b1 b4 == False
    putStr "subBag b4 b1 == False:                               "
    putStrLn $ pass $ subBag b4 b1 == False
    -- check two nonempty bags, no common elements
    putStr "subBag b2 b5 == False:                               "
    putStrLn $ pass $ subBag b2 b5 == False
    putStr "subBag b5 b2 == False:                               "
    putStrLn $ pass $ subBag b5 b2 == False

    -- Test bagToSet
    putStrLn "\nTest bagToSet"
    -- check empty bag
    putStr "bagToSet b0 == []:                                   "
    putStrLn $ pass $ bagToSet b0 == []
    -- check nonempty bag
    putStr "sort (bagToSet b1) == sort (nub s1):                 "
    putStrLn $ pass $ sort (bagToSet b1) == sort (nub s1)

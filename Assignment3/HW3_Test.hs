-- CSCI 450
-- Homework 3
-- Tam Nguyen

module HW3_Test
where

import HW3

main :: IO()
main = 
    do
        putStrLn "3.a"
        putStrLn("sumSqNeg1 []          = " ++ show(sumSqNeg1 []))
        putStrLn("sumSqNeg1 [1,2,3]     = " ++ show(sumSqNeg1 [1,2,3]))
        putStrLn("sumSqNeg1 [-1,-2,-3]  = " ++ show(sumSqNeg1 [-1,-2,-3]))
        putStrLn("sumSqNeg1 [-5,0,5]    = " ++ show(sumSqNeg1 [-5,0,5]))
        putStrLn("")

        putStrLn "3.b"
        putStrLn("sumSqNeg2 []          = " ++ show(sumSqNeg2 []))
        putStrLn("sumSqNeg2 [1,2,3]     = " ++ show(sumSqNeg2 [1,2,3]))
        putStrLn("sumSqNeg2 [-1,-2,-3]  = " ++ show(sumSqNeg2 [-1,-2,-3]))
        putStrLn("sumSqNeg2 [-5,0,5]    = " ++ show(sumSqNeg2 [-5,0,5]))
        putStrLn("")

        putStrLn "3.c"
        putStrLn("sumSqNeg3 []          = " ++ show(sumSqNeg3 []))
        putStrLn("sumSqNeg3 [1,2,3]     = " ++ show(sumSqNeg3 [1,2,3]))
        putStrLn("sumSqNeg3 [-1,-2,-3]  = " ++ show(sumSqNeg3 [-1,-2,-3]))
        putStrLn("sumSqNeg3 [-5,0,5]    = " ++ show(sumSqNeg3 [-5,0,5]))
        putStrLn("")

        putStrLn "3.d"
        putStrLn("sumSqNeg4 []          = " ++ show(sumSqNeg4 []))
        putStrLn("sumSqNeg4 [1,2,3]     = " ++ show(sumSqNeg4 [1,2,3]))
        putStrLn("sumSqNeg4 [-1,-2,-3]  = " ++ show(sumSqNeg4 [-1,-2,-3]))
        putStrLn("sumSqNeg4 [-5,0,5]    = " ++ show(sumSqNeg4 [-5,0,5]))
        putStrLn("")

        putStrLn "4."
        putStrLn("total (*2) 5      = " ++ show(total (*2) 5))
        putStrLn("total (^9) 0      = " ++ show(total (^9) 0))
        putStrLn("total (^3) 8      = " ++ show(total (^3) 8))
        putStrLn("total (`mod` 2) 9 = " ++ show(total (`mod` 2) 9))
        putStrLn("")

        putStrLn "5."
        putStrLn("removeFirst odd []            = " ++ show(removeFirst odd []))
        putStrLn("removeFirst odd [0,1,2,3,4,5] = " ++ show(removeFirst odd [0,1,2,3,4,5]))
        putStrLn("removeFirst (<0) [0,-3,-6,-9] = " ++ show(removeFirst (<0) [0,-3,-6,-9]))
        putStrLn("removeFirst (\\x -> length x > 4) [\"aaaa\",\"bbbbbb\",\"cc\"] = " ++ show(removeFirst (\x -> length x > 4) ["aaaa","bbbbbb","cc"]))
        putStrLn("removeFirst (==5) [5,3,3,3,5] = " ++ show(removeFirst (==5) [5,3,3,3,5]))
        putStrLn("")

        putStrLn "6."
        putStrLn("removeLast odd []            = " ++ show(removeLast odd []))
        putStrLn("removeLast odd [0,1,2,3,4,5] = " ++ show(removeLast odd [0,1,2,3,4,5]))
        putStrLn("removeLast (<0) [0,-3,-6,-9] = " ++ show(removeLast (<0) [0,-3,-6,-9]))
        putStrLn("removeLast (\\x -> length x > 4) [\"aaaa\",\"bbbbbb\",\"ccccc\",\"dd\"] = " ++ show(removeLast (\x -> length x > 4) ["aaaa","bbbbbb","ccccc","dd"]))
        putStrLn("removeLast (==5) [5,3,3,3,5] = " ++ show(removeLast (==5) [5,3,3,3,5]))
        putStrLn("")

        putStrLn "7."
        putStrLn("map2 [(*2), (*3)] []                 = " ++ show(map2 [(*2), (*3)] []))
        --putStrLn("map2 [] [1,2,3]                      = " ++ show(map2 [] [1,2,3]))
        putStrLn("map2 [(*2), (`mod` 3)] [5,10]        = " ++ show(map2 [(*2), (`mod` 3)] [5,10]))
        putStrLn("map2 [(*2), (`mod` 3)] [5,10,15]     = " ++ show(map2 [(*2), (`mod` 3)] [5,10,15]))
        putStrLn("map2 [(*2), (`mod` 3), (+10)] [5,10] = " ++ show(map2 [(*2), (`mod` 3), (+10)] [5,10]))
        putStrLn("")

        putStrLn "8."
        putStrLn("fmap' 3 [(+) 1,(+) 2,(+) 3] = " ++ show(fmap' 3 [(+) 1,(+) 2,(+) 3]))
        putStrLn("fmap' 3 [(*) 2, (+) 2]      = " ++ show(fmap' 3 [(*) 2, (+) 2]))
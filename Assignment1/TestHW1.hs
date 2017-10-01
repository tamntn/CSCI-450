-- CSCI 450
-- Homework 1
-- Tam Nguyen

module TestHW1
where

import Homework1

main :: IO()
main = 
    do
        putStrLn "1."
        putStrLn ("sumsqbig 4 8 2       = " ++ show(sumsqbig 4 8 2))
        putStrLn ("sumsqbig 0 12 9      = " ++ show(sumsqbig 0 12 9))
        putStrLn ("sumsqbig 30 19 28    = " ++ show(sumsqbig 30 19 28))
        putStrLn ""

        putStrLn "3."
        putStrLn ("div23n5 6            = " ++ show(div23n5 6))
        putStrLn ("div23n5 0            = " ++ show(div23n5 0))
        putStrLn ("div23n5 30           = " ++ show(div23n5 30))
        putStrLn ""

        putStrLn "4."
        putStrLn ("notDiv 10 5          = " ++ show(notDiv 10 5))
        putStrLn ("notDiv 22 3          = " ++ show(notDiv 22 3))
        putStrLn ("notDiv 17 1          = " ++ show(notDiv 17 1))
        putStrLn ""

        putStrLn "5."
        putStrLn("mult 3 1              = " ++ show(mult 3 1))
        putStrLn("mult 7 0              = " ++ show(mult 7 0))
        putStrLn("mult 0 10             = " ++ show(mult 0 10))
        putStrLn("mult 5 9              = " ++ show(mult 5 9))
        putStrLn ""

        putStrLn "6."
        putStrLn ("addTax 2.0 9.0       = " ++ show(addTax 2.0 9.0))
        putStrLn ("subTax 2.18 9.0      = " ++ show(subTax 2.18 9.0))
        putStrLn ("addTax 100 9.5       = " ++ show(addTax 100 9.5))
        putStrLn ("subTax (addTax 100 9.5) 9.5      = " ++ show(subTax (addTax 100 9.5) 9.5))
        putStrLn ""

        putStrLn "7."
        putStrLn ("comesBefore (10,30,\"AM\") (9,15,\"PM\")    = " ++ show(comesBefore (10,30,"AM") (9,15,"PM")))
        putStrLn ("comesBefore (10,30,\"PM\") (9,15,\"PM\")    = " ++ show(comesBefore (10,30,"PM") (9,15,"PM")))
        putStrLn ("comesBefore (7,45,\"PM\") (9,15,\"AM\")     = " ++ show(comesBefore (7,45,"PM") (9,15,"AM")))
        putStrLn ("comesBefore (5,30,\"AM\") (7,15,\"AM\")     = " ++ show(comesBefore (5,30,"AM") (7,15,"AM")))
        putStrLn ("comesBefore (6,30,\"PM\") (6,30,\"PM\")     = " ++ show(comesBefore (6,30,"PM") (6,30,"PM")))

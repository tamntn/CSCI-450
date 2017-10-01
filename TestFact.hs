{-  Factorial Test Module (Int)
    Section 2.2 of Basic Haskell Functional Programming notes
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

    2017-01-27: Revised for module
    2017-02-03: Added comments
    2017-08-27: Expanded testing, included beyond fact1 .. fact6

    The tests marked OMIT generate error returns. They need to be
    tested, but must be done separately from this simple script.

-}

module TestFact
where

import Fact

main :: IO ()
main =
    do
        putStrLn ("fact1 0      = " ++ show (fact1 0))
        putStrLn ("fact1 5      = " ++ show (fact1 5))
        putStrLn ("fact1 20     = " ++ show (fact1 20))
        putStrLn ("fact1 21     = " ++ show (fact1 21))
--OMIT  putStrLn ("fact1 (-1)   = " ++ show (fact1 (-1)))
        putStrLn ""
        
        putStrLn ("fact2 0      = " ++ show (fact2 0))
        putStrLn ("fact2 5      = " ++ show (fact2 5))
        putStrLn ("fact2 20     = " ++ show (fact2 20))
        putStrLn ("fact2 21     = " ++ show (fact2 21))
--OMIT  putStrLn ("fact2 (-1)   = " ++ show (fact2 (-1)))
        putStrLn ""
        
        putStrLn ("fact3 0      = " ++ show (fact3 0))
        putStrLn ("fact3 5      = " ++ show (fact3 5))
        putStrLn ("fact3 20     = " ++ show (fact3 20))
        putStrLn ("fact3 21     = " ++ show (fact3 21))
--OMIT  putStrLn ("fact3 (-1)   = " ++ show (fact3 (-1)))
        putStrLn ""
        
        putStrLn ("fact4 0      = " ++ show (fact4 0))
        putStrLn ("fact4 5      = " ++ show (fact4 5))
        putStrLn ("fact4 20     = " ++ show (fact4 20))
        putStrLn ("fact4 21     = " ++ show (fact4 21))
--OMIT  putStrLn ("fact4 (-1)   = " ++ show (fact4 (-1)))
        putStrLn ""
        
        putStrLn ("fact4' 0     = " ++ show (fact4' 0))
        putStrLn ("fact4' 5     = " ++ show (fact4' 5))
        putStrLn ("fact4' 20    = " ++ show (fact4' 20))
        putStrLn ("fact4' 21    = " ++ show (fact4' 21))
--OMIT  putStrLn ("fact4' (-1)  = " ++ show (fact4' (-1)))
        putStrLn ""
        
        putStrLn ("fact4'' 0    = " ++ show (fact4'' 0))
        putStrLn ("fact4'' 5    = " ++ show (fact4'' 5))
        putStrLn ("fact4'' 20   = " ++ show (fact4'' 20))
        putStrLn ("fact4'' 21   = " ++ show (fact4'' 21))
--OMIT  putStrLn ("fact4'' (-1) = " ++ show (fact4'' (-1)))
        putStrLn ""
        
        putStrLn ("fact5 0      = " ++ show (fact5 0))
        putStrLn ("fact5 5      = " ++ show (fact5 5))
        putStrLn ("fact5 20     = " ++ show (fact5 20))
        putStrLn ("fact5 21     = " ++ show (fact5 21))
        putStrLn ("fact5 (-1)   = " ++ show (fact5 (-1)))
        putStrLn ""
        
        putStrLn ("fact6 0      = " ++ show (fact6 0))
        putStrLn ("fact6 5      = " ++ show (fact6 5))
        putStrLn ("fact6 20     = " ++ show (fact6 20))
        putStrLn ("fact6 21     = " ++ show (fact6 21))
--OMIT  putStrLn ("fact6 (-1)   = " ++ show (fact6 (-1)))
        putStrLn ""

        putStrLn ("fact7 0      = " ++ show (fact7 0))
        putStrLn ("fact7 5      = " ++ show (fact7 5))
        putStrLn ("fact7 20     = " ++ show (fact7 20))
        putStrLn ("fact7 21     = " ++ show (fact7 21))
--OMIt  putStrLn ("fact7 (-1)   = " ++ show (fact7 (-1)))
        putStrLn ""

        putStrLn ("fact8 0      = " ++ show (fact8 0))
        putStrLn ("fact8 5      = " ++ show (fact8 5))
        putStrLn ("fact8 20     = " ++ show (fact8 20))
        putStrLn ("fact8 21     = " ++ show (fact8 21))
        putStrLn ("fact8 (-1)   = " ++ show (fact8 (-1)))
        putStrLn ""

        putStrLn ("fact9 0      = " ++ show (fact9 0))
        putStrLn ("fact9 5      = " ++ show (fact9 5))
        putStrLn ("fact9 20     = " ++ show (fact9 20))
        putStrLn ("fact9 21     = " ++ show (fact9 21))
        putStrLn ("fact9 (-1)   = " ++ show (fact9 (-1)))
        putStrLn ""

        putStrLn ("fact10 0     = " ++ show (fact10 0))
        putStrLn ("fact10 5     = " ++ show (fact10 5))
        putStrLn ("fact10 20    = " ++ show (fact10 20))
        putStrLn ("fact10 21    = " ++ show (fact10 21))
        putStrLn ("fact10 (-1)  = " ++ show (fact10 (-1)))
        putStrLn ""

        putStrLn ("fact11 0     = " ++ show (fact11 0))
        putStrLn ("fact11 5     = " ++ show (fact11 5))
        putStrLn ("fact11 20    = " ++ show (fact11 20))
        putStrLn ("fact11 21    = " ++ show (fact11 21))
        putStrLn ("fact11 (-1)  = " ++ show (fact11 (-1)))
        putStrLn ""





module Mod05HigherOrder
  ( )
where
        import Data.List ( foldl' )

        squareAll :: [Int] -> [Int] 
        squareAll []     = [] 
        squareAll (x:xs) = (x * x) : squareAll xs


        lengthAll :: [[a]] -> [Int] 
        lengthAll []       = [] 
        lengthAll (xs:xss) = (length xs) : lengthAll xss

        map' :: (a -> b) -> [a] -> [b]  -- map in Prelude
        map' f []     = [] 
        map' f (x:xs) = f x : map' f xs 


        squareAll2 :: [Int] -> [Int] 
        squareAll2 xs = map' sq xs 
                        where sq x = x * x 

        lengthAll2 :: [[a]] -> [Int] 
        lengthAll2 xss = map' length xss

        getEven :: [Int] -> [Int] 
        getEven []            = [] 
        getEven (x:xs)
                  | even x    = x : getEven xs 
                  | otherwise = getEven xs

        doublePos :: [Int] -> [Int] 
        doublePos []          = [] 
        doublePos (x:xs) 
                  | 0 < x     = (2 * x) : doublePos xs 
                  | otherwise = doublePos xs

        filter' :: (a -> Bool) -> [a] -> [a]  -- filter in Prelude
        filter' _ []    = [] 
        filter' p (x:xs)
            | p x       = x : xs' 
            | otherwise = xs' 
                        where xs' = filter' p xs

        getEven2 :: [Int] -> [Int] 
        getEven2 xs = filter' even xs 

        doublePos2 :: [Int] -> [Int] 
        doublePos2 xs = map' dbl (filter' pos xs) 
                        where dbl x  = 2 * x
                              pos x = (0 < x)

        sum' :: [Int] -> Int             -- sum in Prelude
        sum' []     = 0
        sum' (x:xs) = x + sum' xs 

        product' :: [Integer] -> Integer -- product in Prelude
        product' []     = 1 
        product' (x:xs) = x * product' xs 

        concat' :: [[a]] -> [a]  -- concat in Prelude
        concat' []       =  []
        concat' (xs:xss) =  xs ++ concat' xss

        foldrX :: (a -> b -> b) -> b -> [a] -> b  -- foldr in Prelude
        foldrX f z []     = z
        foldrX f z (x:xs) = f x (foldrX f z xs) 

        sum2 :: [Int] -> Int             -- sum
        sum2 xs = foldrX (+) 0 xs

        product2 :: [Int] -> Int         -- product
        product2 xs = foldrX (*) 1 xs

        concat2:: [[a]] -> [a]           -- concat
        concat2 xss = foldrX (++) [] xss

        and', or' :: [Bool] -> Bool  -- and, or in Prelude
        and' xs = foldrX (&&) True xs 
        or'  xs = foldrX (||) False xs

        map2 :: (a -> b) -> [a] -> [b] -- map
        map2 f xs = foldr mf [] xs 
            where mf y ys = (f y) : ys
 
        filter2 :: (a -> Bool) -> [a] -> [a]  -- filter
        filter2 p xs = foldr ff [] xs
            where ff y ys = if p y then (y:ys) else ys

        length2 :: [a] -> Int  -- length
        length2 xs  = foldr len  0  xs
            where len _ acc = acc + 1

        append2 :: [a] -> [a] -> [a]  -- ++
        append2 xs ys = foldr (:) ys xs

        foldlX :: (a -> b -> a) -> a -> [b] -> a  -- foldl in Prelude
        foldlX f z []     = z  
        foldlX f z (x:xs) = foldlX f (f z x) xs

        sum3, product3 :: Num a =>  [a] -> a -- sum, product
        sum3 xs     = foldl' (+) 0 xs
        product3 xs = foldl' (*) 1 xs

        length3 :: [a] -> Int  -- length
        length3 xs  = foldl len 0  xs
            where len acc _ = acc + 1

        reverse2 :: [a] -> [a]  -- reverse
        reverse2 xs = foldl rev [] xs
            where rev acc x = (x:acc) 

        foldr2 :: (a -> b -> b) -> b -> [a] -> b  -- foldr
        foldr2 f z xs = foldl flipf z (reverse xs)
            where flipf y x = f x y

        concatMap' :: (a -> [b]) -> [a] -> [b] 
        concatMap' f xs = concat(map f xs)

        concatMap2 :: (a -> [b]) -> [a] -> [b] 
        concatMap2 f xs = foldr fmf [] xs 
            where fmf x ys = f x ++ ys

        filter3 :: (a -> Bool) -> [a] -> [a]
        filter3 p xs = concatMap fmf xs
            where fmf x = if p x then [x] else []

        two :: a -> Int 
        two x = 2


-- definitions of && and || changed to &&& and ||| to avoid conflict

        (&&&), (|||) :: Bool -> Bool -> Bool 
        False &&& x = False  -- second argument not evaluated 
        True  &&& x = x 

        False ||| x = x 
        True  ||| x = True   -- second argument not evaluated

        add :: (Int,Int) -> Int 
        add (x,y) = x + y 

        add' :: Int -> (Int -> Int) 
        add' x y  = x + y 

        doublePos3 :: [Int] -> [Int] 
        doublePos3 xs = map' ((*) 2) (filter' ((<) 0) xs) 

        f, g :: a -> a
        f x = x

        g x = f x

        g' = f

        flip' :: (a -> b -> c) -> b -> a -> c  -- flip in Prelude
        flip' f x y = f y x

        sumCubes :: [Int] -> Int 
        sumCubes xs = sum' (map' (^3) xs)

--      sum = foldl' (+) 0  -- sum

        const' :: a -> b -> a  -- const in Prelude
        const' k x = k

        id' :: a -> a  -- id in Prelude
        id' x = x


        fst' :: (a,b) -> a  -- fst in Prelude
        fst' (x,_) = x 

        snd' :: (a,b) -> b  -- snd in Prelude
        snd' (_,y) = y

        reverse' :: [a] -> [a]          -- reverse in Prelude
        reverse' = foldlX (flip' (:)) []
        
        curry' :: ((a, b) -> c) -> a -> b -> c
        curry' f x y =  f (x, y)

        uncurry' :: (a -> b -> c) -> ((a, b) -> c)
        uncurry' f p =  f (fst p) (snd p)


        fork :: (a -> b, a -> c) -> a -> (b,c)
        fork (f,g) x = (f x, g x)

        cross :: (a -> b, c -> d) -> (a,c) -> (b,d)
        cross (f,g) (x,y) = (f x, g y)
 
        
-- commented out for complation
--      infixr 9 .
--      (.) :: (b -> c) -> (a -> b) -> (a -> c) 
--      (f . g) x = f (g x) 
--
--      doit x = f1 (f2 (f3 (f4 x))) 
--
--      doit = f1 . f2 . f3 . f4 

        count :: Int -> [[a]] -> Int 
        count n 
            | n >= 0    = length . filter' (== n) . map' length 
            | otherwise = const' 0   -- discard 2nd arg, return 0 

        doublePos4 :: [Int] -> [Int] 
        doublePos4 = map' (2*) . filter' (0<)

        last' = head . reverse'            -- last in Prelude
        init' = reverse' . tail . reverse' -- init in Prelude

        last2 :: [a] -> a    -- last in Prelude
        last2 [x]    = x 
        last2 (_:xs) = last2 xs 

        init2 :: [a] -> [a]  -- init in Prelude
        init2 [x]    = [] 
        init2 (x:xs) = x : init2 xs 

        any', all' :: (a -> Bool) -> [a] -> Bool 
        any' p = or' . map' p   -- any in Prelude
        all' p = and' . map' p  -- all in Prelude

        elem', notElem' :: Eq a => a -> [a] -> Bool 
        elem'    = any' . (==)  -- elem in Prelude
        notElem' = all' . (/=)  -- notElem in Prelude

        squareAll3 :: [Int] -> [Int] 
        squareAll3 xs = map (\x -> x * x) xs 

        length4 :: [a] -> Int   -- length in Prelude
        length4  = foldl' (\n _ -> n+1) 0

        takeWhile':: (a -> Bool) -> [a] -> [a] -- takeWhile in Prelude
        takeWhile'  p []  = [] 
        takeWhile'  p (x:xs) 
            | p x        = x : takeWhile' p xs 
            | otherwise  = []

        dropWhile' :: (a -> Bool) -> [a] -> [a] -- dropWhile in Prelude
        dropWhile'  p []  = [] 
        dropWhile'  p xs@(x:xs') 
            | p x        = dropWhile' p xs' 
            | otherwise  = xs

        zipWith' :: (a->b->c) -> [a]->[b]->[c] -- zipWith in Prelude
        zipWith' z  (x:xs)  (y:ys) = z x y : zipWith' z xs ys 
        zipWith' _  _     _        = []

        zip'' :: [a] -> [b] -> [(a,b)] 
        zip'' = zipWith' (\x y -> (x,y)) 

        zip''' :: [a] -> [b] -> [(a,b)] 
        zip''' = zipWith' (,) 

        sp :: Num a => [a] -> [a] -> a 
        sp xs ys = sum (zipWith' (*) xs ys) 

        -- added for compilation
        type Rat = (Int,Int)

        compareRat :: (Int -> Int -> Bool) -> Rat -> Rat -> Bool 
        compareRat r (a,b) (c,d) 
            |  b == 0    = errRat (a,0) 
            |  d == 0    = errRat (c,0) 
            |  otherwise = r (a*d) (b*c) 

        eqRat,neqRat,ltRat,leqRat,gtRat,geqRat :: Rat -> Rat -> Bool 
        eqRat   = compareRat (==) 
        neqRat  = compareRat (/=) 
        ltRat   = compareRat (<)
        leqRat  = compareRat (<=) 
        gtRat   = compareRat (>)
        geqRat  = compareRat (>=)

        -- added from module 3 code
        errRat (x,y) = error ("Invalid rational number" 
                      ++ showRat (x,y))

        showRat :: Rat -> String
        showRat (a,b) = show a ++ "/" ++ show b

        msort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
        msort _    [] = []
        msort less xs = merge (msort less ls) (msort less rs)
            where  n       = (length xs) `div` 2 
                   (ls,rs) = splitAt n xs
                   merge [] ys         = ys
                   merge xs []         = xs
                   merge ls@(x:xs) rs@(y:ys) 
                       | less x y      = x : merge xs rs
                       | otherwise     = y : merge ls ys

        descendSort :: Ord a => [a] -> [a]
        descendSort = msort (\ x y -> x > y)

--      gmerge :: [a] -> [b] -> [c]
--      gmerge []  []         = e1 
--      gmerge [] bs@(y:ys)   = e2 bs
--      gmerge as@(x:xs) []   = e3 as
--      gmerge as@(x:xs) bs@(y:ys)
--        | keya x &lt keyb y = f4 x y ++ gmerge (g4 as) (h4 bs)
--        | keya x == keyb y  = f5 x y ++ gmerge (g5 as) (h5 bs)
--        | keya x > keyb y   = f6 x y ++ gmerge (g6 as) (h6 bs)

        gmerge :: Ord d => 
            (a -> d) ->        -- keya 
            (b -> d) ->        -- keyb 
            [c] ->             -- e1 
            ([b] -> [c]) ->    -- e2 
            ([a] -> [c]) ->    -- e3 
            (a -> b -> [c]) -> -- f4 
            (a -> b -> [c]) -> -- f5 
            (a -> b -> [c]) -> -- f6 
            ([a] -> [a]) ->    -- g4 
            ([a] -> [a]) ->    -- g5 
            ([a] -> [a]) ->    -- g6 
            ([b] -> [b]) ->    -- h4 
            ([b] -> [b]) ->    -- h5 
            ([b] -> [b]) ->    -- h6 
            [a] -> [b] -> [c] 
        gmerge keya keyb e1 e2 e3 f4 f5 f6 g4 g5 g6 h4 h5 h6 
            = gmerge'
              where 
                gmerge'  []  []        = e1 
                gmerge'  []  bs@(y:ys) = e2 bs 
                gmerge'  as@(x:xs)  [] = e3 as 
                gmerge'  as@(x:xs)  bs@(y:ys) 
                    | keya x <  keyb y = f4 x y ++ gmerge' (g4 as) (h4 bs)
                    | keya x == keyb y = f5 x y ++ gmerge' (g5 as) (h5 bs)
                    | keya x >  keyb y = f6 x y ++ gmerge' (g6 as) (h6 bs)


        merge1 :: Ord a => [a] -> [a] -> [a]
        merge1 []        []        = []
        merge1 []        bs@(y:ys) = bs 
        merge1 as@(x:xs) []        = as 
        merge1 as@(x:xs) bs@(y:ys) 
            | x <  y               = x : merge1 xs bs  
            | x == y               = x : merge1 xs bs  
            | x >  y               = y : merge1 as ys 


        merge2 :: Ord a => [a] -> [a] -> [a]
        merge2 []        bs        = bs 
        merge2 as        []        = as 
        merge2 as@(x:xs) bs@(y:ys) 
            | x <= y               = x : merge2 xs bs 
            | x >  y               = y : merge2 as ys 

        intersect :: Ord a => [a] -> [a] -> [a]
        intersect []        _         = []  -- discard any remaining 
        intersect _         []        = []  -- discard any remaining 
        intersect as@(x:xs) bs@(y:ys) 
            | x == y                  = x : intersect xs ys  -- keep match 
            | x <  y                  = intersect xs bs  -- discard smaller 
            | x >  y                  = intersect as ys  -- discard smaller 

        merge1' :: Ord a => [a] -> [a] -> [a] 
        merge1' = gmerge  id id     -- keya, keyb 
           [] id id                 -- e1, e2, e3 
           (const . (:[]))          -- f4 
           (const . (:[]))          -- f5 
           (flip (const . (:[])))   -- f6 
           tail tail id             -- g4, g5, g6        
           id id tail               -- h4, h5, h6

        intersect' :: Ord a => [a] -> [a] -> [a]
        intersect' = gmerge id id   -- keya, keyb
           [] (const []) (const []) -- e1, e2, e3
           (\ x y -> [])            -- f4
           (const . (:[]))          -- f5
           (\ x y -> [])            -- f6
           tail tail id             -- g4, g5, g6
           id   tail tail           -- h4, h5, h6


        foldlP :: (a -> b -> a) -> a -> [b] -> a  -- foldl' in Data.List 
        foldlP  f z []    = z 
        foldlP f z (x:xs) = y `seq` foldlP f y xs 
                            where y = f z x 

        foldlQ :: (a -> b -> a) -> a -> [b] -> a  -- foldl' in Data.List 
        foldlQ f z []     = z 
        foldlQ f z (x:xs) = (foldlQ f $! f z x) xs

        sum4 :: [Integer] -> Integer  
        sum4 xs = sumIter xs 0
            where sumIter []     acc = acc
                  sumIter (x:xs) acc = sumIter xs (acc+x)

        sum5 :: [Integer] -> Integer  
        sum5 xs = sumIter xs 0
            where sumIter []     acc = acc
                  sumIter (x:xs) acc = sumIter xs $! acc + x


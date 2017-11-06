module Chap08
where

    import Data.List

 -- 8.1 User-defined types
    data Color = Red | White | Blue 
                 deriving (Show, Eq)

    isRed :: Color -> Bool 
    isRed Red = True
    isRed _   = False

    test01 = isRed Red   == True
    test02 = isRed White == False
    test03 = isRed Blue  == False

    data Color' = Red' | Blue' | Grayscale Int
                  deriving (Show, Eq)

    data Point a = Pt a a
                   deriving (Show, Eq)

    data Set a = Set [a]
                 deriving (Show, Eq)

    makeSet :: Eq a => [a] -> Set a 
    makeSet xs = Set (nub xs)

    test04 = makeSet [3,3,2,1,2] == Set [3,2,1]

    type Matrix a = [[a]]

    data Result a = Ok a | Err String
                    deriving (Show, Eq)

    isErr (Err xs) = True
    isErr _        = False

    divide :: Int -> Int -> Result Int 
    divide _  0 = Err "Divide by zero" 
    divide x  y = Ok (x `div` y)

    test05 = isErr (divide 27 0) == True
    test06 = divide 27 3 == Ok 9

    f :: Int -> Int -> Int 
    f x y  = return (divide x y) 
             where return (Ok z)  = z 
                   return (Err s) = maxBound

    test07 = f 27 3 == 9
    test08 = f 27 0 == maxBound

    f' x y = case divide x y of 
                  Ok z  -> z 
                  Err s -> maxBound

    test09 = f' 27 3 == 9
    test10 = f' 27 0 == maxBound

-- 8.2 Recursive Data Types

    data BinTree a = Empty | Node (BinTree a) a (BinTree a)
                     deriving (Show, Eq)

    flatten :: BinTree a -> [a] 
    flatten Empty        = [] 
    flatten (Node l v r) = flatten l ++ [v] ++ flatten r

    atree = (Node (Node Empty 3 Empty) 5 
            (Node (Node Empty 7 Empty) 1 Empty))

    test11 = flatten atree == [3,5,7,1]

    flatten' :: BinTree a -> [a] 
    flatten' t = inorder t [] 
                 where inorder Empty xs        = xs 
                       inorder (Node l v r) xs = 
                           inorder l (v : inorder r xs)

    test12 = flatten' atree == [3,5,7,1]

    treeFold :: (a -> a -> a) -> a -> BinTree a -> a 
    treeFold op i Empty	       = i 
    treeFold op i (Node l v r) = op (op (treeFold op i l) v) 
                                    (treeFold op i r)
    
    test13 = (treeFold (+) 0 atree == 16)

    data Tree a = Leaf a | Tree a :^: Tree a
                  deriving (Show, Eq)

    ltree = ((Leaf 1 :^: Leaf 2) :^: (Leaf 3 :^: Leaf 4))

    fringe :: Tree a -> [a]  
    fringe (Leaf v)  = [v] 
    fringe (l :^: r) = fringe l ++ fringe r 

    test14 = fringe ltree == [1,2,3,4]
   
    fringe' :: Tree a -> [a] 
    fringe' t = leaves t [] 
                where leaves (Leaf v)  = ((:) v) 
                      leaves (l :^: r) = leaves l . leaves r

    test15 = fringe' ltree == [1,2,3,4]

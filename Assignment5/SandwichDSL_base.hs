{- CSci 450/503, Fall 2017
   Homework #5: Sandwich DSL base
   H. Conrad Cunningham
   27 October 2017

1234567890123456789012345678901234567890123456789012345678901234567890

This is the SandwichDSL base code from the case study. It can be
expanded to build the module for Assignment #5.

-}

module SandwichDSL_base
  -- ( Platter(..), Sandwich(..), Layer(..), Bread(..), Meat(..),
  --   Cheese(..), Vegetable(..), Condiment(..), SandwichOP(..),
  --   Program(..), PriceList, etc. ...
  -- )
where

-- Used functions from these modules in my implementation
import Data.Maybe
import Data.List

{- Haskell data type definitions from "Building the DSL" -}

data Platter   = Platter [Sandwich] 
                 deriving Show

data Sandwich  = Sandwich [Layer]
                 deriving Show

data Layer     = Bread Bread         | Meat Meat           |
                 Cheese Cheese       | Vegetable Vegetable | 
                 Condiment Condiment
                 deriving (Eq,Show)

data Bread     = White | Wheat | Rye
                 deriving (Eq,Show)

data Meat      = Turkey | Chicken | Ham | RoastBeef | Tofu
                 deriving (Eq,Show)

data Cheese    = American | Swiss | Jack | Cheddar
                 deriving (Eq,Show)

data Vegetable = Tomato | Onion | Lettuce | BellPepper
                 deriving (Eq,Show)

data Condiment = Mayo | Mustard | Ketchup | Relish | Tabasco
                 deriving (Eq,Show)

type PriceList = [(Layer,Int)]

-- Function type signatures given in section
-- newSandwich :: Bread -> Sandwich
-- addLayer ::    Sandwich -> Layer -> Sandwich
-- newPlatter ::  Platter
-- addSandwich :: Platter -> Sandwich -> Platter
-- priceSandwich :: PriceList -> Sandwich -> Int

{- Suggested test data for PriceList
prices = [ (Bread White, 20), (Bread Wheat, 30), 
           (Bread Rye, 30), 
           (Meat Turkey, 100), (Meat Chicken, 80), 
           (Meat Ham, 120), (Meat RoastBeef, 140), 
           (Meat Tofu, 50), 
           (Cheese American, 50), (Cheese Swiss, 60), 
           (Cheese Jack, 60), (Cheese Cheddar, 60), 
           (Vegetable Tomato, 25), (Vegetable Onion, 20), 
           (Vegetable Lettuce, 20), (Vegetable BellPepper, 25), 
           (Condiment Mayo, 5), (Condiment Mustard, 4), 
           (Condiment Ketchup, 4), (Condiment Relish, 10), 
           (Condiment Tabasco, 5) 
         ]
-}


{- Haskell data type definitions from 
   "Compiling the Program for the SueChef Controller"
-}

data SandwichOp = StartSandwich    | FinishSandwich
                | AddBread Bread   | AddMeat Meat
                | AddCheese Cheese | AddVegetable Vegetable
                | AddCondiment Condiment
                | StartPlatter | MoveToPlatter | FinishPlatter 
                  deriving (Eq, Show) 

data Program = Program [SandwichOp]
               deriving Show

-- compileSandwich :: Sandwich -> [SandwichOp]
-- compile :: Platter -> Program 
   
------------ End of SandwichDSL_base ----------


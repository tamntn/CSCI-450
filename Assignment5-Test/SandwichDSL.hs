module SandwichDSL

where
-- Used functions from these modules in my implementation	
import SandwichDSL_base
import Data.Maybe
import Data.List


--1 Define and implement the Haskell functions newSandwich, addLayer, newPlatter, and addSandwich described above.
newSandwich :: Bread -> Sandwich
newSandwich b = Sandwich [Bread b]

addLayer :: Sandwich -> Layer -> Sandwich
addLayer (Sandwich head) layer = Sandwich(layer : head)

newPlatter ::  Platter
newPlatter = Platter []

addSandwich :: Platter -> Sandwich -> Platter
addSandwich (Platter platter) sandwich = Platter(sandwich : platter)


--2.	 Define and implement the Haskell query functions below that take an ingredient (i.e., Layer) and return  
--		 True if and only if the ingredient is in the specified category.

isBread :: Layer -> Bool
isBread (Bread b)           = True
isBread notBread            = False

isMeat :: Layer -> Bool
isMeat (Meat b)            = True
isMeat notMeat             = False

isCheese :: Layer -> Bool
isCheese (Cheese b)        = True
isCheese notCheese         = False

isVegetable :: Layer -> Bool
isVegetable (Vegetable b)  = True
isVegetable notVegetable   = False

isCondiment :: Layer -> Bool
isCondiment (Condiment b)  = True
isCondiment notCondiment   = False

-- 3. 	Define and implement a Haskell function noMeat that takes a sandwich 
--		and returns True if and only if the sandwich contains no meats.
noMeat :: Sandwich -> Bool
noMeat (Sandwich meat) = filter isMeat meat == []

{-- 4.According to a proposed City of Oxford ordinance, in the future it may be necessary to assemble all sandwiches in Oxford Standard Order (OSO): 
a slice of bread on the bottom, then zero or more meats layered above that, then zero or more cheeses, then zero or more vegetables, then zero or more condiments,
 and then a slice of bread on top. The top and bottom slices of bread must be of the same type.

Define and implement a Haskell function inOSO that takes a sandwich and determines whether it is in OSO and 
another function intoOSO that takes a sandwich and a default bread and 
returns the sandwich with the same ingredients ordered in OSO 
--}
isnotBread :: Layer -> Bool
isnotBread (Bread b)           = False
isnotBread notBread            = True


inOSO   :: Sandwich -> Bool
inOSO (Sandwich layers) = inOSOaux layers (length (filter isBread layers))

inOSOaux :: [Layer] -> Int-> Bool
inOSOaux (x:xs) len 
 | len <=1 = False
 |len > 1= (isBread(x) && isBread(last (x:xs)) && x == last xs && (len==2))


intoOSO :: Sandwich -> Bread -> Sandwich
intoOSO (Sandwich layers) b  
 |length (filter isBread layers)==0 = Sandwich (concate(filter isnotBread layers) (Bread b))
 |length (filter isBread layers)>0  = Sandwich (concate(filter isnotBread layers) (head (filter isBread layers)))

concate ::[Layer]-> Layer ->[Layer]
concate [] b = b:reverse(b:[])
concate (x:xs ) b= b:x:reverse(b:xs)


--5 Suppose we store the current prices of the sandwich ingredients in an association list with the following type synonym:
priceSandwich :: PriceList ->Integer -> Sandwich -> Integer
priceSandwich pricelist base (Sandwich layers) = total (layers) base
        where total :: [Layer] -> Integer -> Integer
              total [] base   = base
              total (x:xs) sum = total xs (search + sum)
                               where search = fromJust(lookup x pricelist)


{-- 6. Define and implement a Haskell function eqSandwich that compares two sandwiches for equality. 
Although the definition of equality could differ, use "bag equality". That is, 
two sandwiches are equal if they have the same number of layers (zero or more) of each ingredient, regardless of the order of the layers.
--}
eqSandwich :: Sandwich -> Sandwich -> Bool
eqSandwich (Sandwich sandwich1) (Sandwich sandwich2) = sandwich1 `intersect` sandwich2 == sandwich1


-- 7. Give the Haskell declaration needed to make Sandwich an instance of class Eq. You may use eqSandwich if applicable.
class Eq a where
equalSandwich :: Sandwich -> Sandwich -> Bool
equalSandwich (Sandwich sandwich1)  (Sandwich sandwich2) =  eqSandwich (Sandwich sandwich1) (Sandwich sandwich2)
import SandwichDSL
import SandwichDSL_base

main = do
putStrLn "-----------------------------------------------------" 
print("1. newSandwich ");
putStrLn(" ")
putStr " newSandwich White"
putStrLn ""
putStrLn (show (newSandwich White))
putStrLn ""
putStr "newSandwich Wheat"
putStrLn ""
putStrLn (show (newSandwich Wheat))
putStrLn ""
putStr "newSandwich Rye"
putStrLn ""
putStrLn (show (newSandwich Rye))

putStrLn "-----------------------------------------------------" 
print("1. addLayer ");
putStr "addLayer (newSandwich White)(Meat Turkey)"
putStrLn ""
putStrLn (show (addLayer (newSandwich White)(Meat Turkey)))
putStrLn ""        
putStr "addLayer (newSandwich White)(Cheese Jack)"
putStrLn ""
putStrLn (show (addLayer (newSandwich White)(Cheese Jack)))
putStrLn "" 
putStr "addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion)"
putStrLn ""
putStrLn (show (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion)))
putStrLn "-----------------------------------------------------" 

print("1. newPlatter ");
putStr "newPlatter"
putStrLn ""
putStrLn (show (newPlatter))
putStrLn ""
putStrLn "-----------------------------------------------------" 

print("1. addSandwich ");
putStr "addSandwich (newPlatter)(newSandwich White)"
putStrLn ""
putStrLn (show (addSandwich (newPlatter)(newSandwich White)))
putStrLn ""        
putStr "addSandwich (newPlatter)(Sandwich [Vegetable Onion,Meat Turkey,Bread White])"
putStrLn ""
putStrLn (show (addSandwich (newPlatter)(Sandwich [Vegetable Onion,Meat Turkey,Bread White])))
putStrLn "" 
---------------------------
putStr "addSandwich (newPlatter) (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion))"
putStrLn ""
putStrLn (show (addSandwich (newPlatter) (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion))))
putStrLn "-----------------------------------------------------" 

putStrLn "2 isBread"
putStrLn ""
putStr " isBread (Bread White)"
putStrLn ""
putStrLn (show (isBread (Bread White)))
putStrLn ""
putStr " isBread (Cheese Jack)"
putStrLn ""
putStrLn (show (isBread (Cheese Jack)))
putStrLn ""      
putStr "isBread (Meat Ham)"
putStrLn ""
putStrLn (show (isBread (Meat Ham)))
putStrLn ""
putStr "isBread (Vegetable Onion)"
putStrLn ""
putStrLn (show (isBread (Vegetable Onion)))
putStrLn ""
putStr  "isBread (Condiment Mayo)"
putStrLn ""
putStrLn (show (isBread (Condiment Mayo)))
putStrLn ""   
putStrLn "-----------------------------------------------------" 

putStrLn "2 isMeat"
putStrLn ""
putStr "isMeat (Bread White)"
putStrLn ""
putStrLn (show (isMeat (Bread White)))
putStrLn ""
putStr "isMeat (Cheese Jack)"
putStrLn ""
putStrLn (show (isMeat (Cheese Jack)))
putStrLn ""      
putStr "isMeat (Meat Ham)"
putStrLn ""
putStrLn (show (isMeat (Meat Ham)))
putStrLn ""
putStr "isMeat (Vegetable Onion)"
putStrLn ""
putStrLn (show (isMeat (Vegetable Onion)))
putStrLn ""
putStr " isMeat (Condiment Mayo)"
putStrLn ""
putStrLn (show (isMeat (Condiment Mayo)))
putStrLn ""
putStrLn "-----------------------------------------------------" 


putStrLn "2 isCheese"
putStrLn ""
putStr "isCheese (Bread White)"
putStrLn ""
putStrLn (show (isCheese (Bread White)))
putStrLn ""
putStr "isCheese (Cheese Jack)"
putStrLn ""
putStrLn (show (isCheese (Cheese Jack)))
putStrLn ""    
putStr "isCheese (Meat Ham)"
putStrLn ""
putStrLn (show (isCheese (Meat Ham)))
putStrLn ""
putStr "isCheese (Vegetable Onion)"
putStrLn ""
putStrLn (show (isCheese (Vegetable Onion)))
putStrLn ""
putStr " isCheese (Condiment Mayo)"
putStrLn ""
putStrLn (show (isCheese (Condiment Mayo)))
putStrLn ""   
putStrLn "-----------------------------------------------------" 

putStrLn "2 isVegetable"
putStrLn ""
putStr " isVegetable (Bread White)"
putStrLn ""
putStrLn (show (isVegetable (Bread White)))
putStrLn ""
putStr " isVegetable (Cheese Jack)"
putStrLn ""
putStrLn (show (isVegetable (Cheese Jack)))
putStrLn ""     
putStr "isVegetable (Meat Ham)"
putStrLn ""
putStrLn (show (isVegetable (Meat Ham)))
putStrLn ""
putStr "isVegetable (Vegetable Onion)"
putStrLn ""
putStrLn (show (isVegetable (Vegetable Onion)))
putStrLn ""  
putStr " isVegetable (Condiment Mayo)"
putStrLn ""
putStr "    "
putStrLn (show (isVegetable (Condiment Mayo)))
putStrLn "" 
putStrLn "-----------------------------------------------------" 

putStrLn "2 isCondiment"
putStrLn ""
putStr "isCondiment (Bread White)"
putStrLn ""
putStrLn (show (isCondiment (Bread White)))
putStrLn ""
putStr "isCondiment (Cheese Jack)"
putStrLn ""
putStrLn (show (isCondiment (Cheese Jack)))
putStrLn ""     
putStr "isCondiment (Meat Ham)"
putStrLn ""
putStrLn (show (isCondiment (Meat Ham)))
putStrLn ""
putStr "isCondiment (Vegetable Onion)"
putStrLn ""
putStrLn (show (isCondiment (Vegetable Onion)))
putStrLn ""
putStr "isCondiment (Condiment Mayo)"
putStrLn ""
putStrLn (show (isCondiment (Condiment Mayo)))
putStrLn ""
putStrLn "-----------------------------------------------------" 

putStrLn "3 noMeat"
putStrLn ""
putStr "noMeat (Sandwich [Meat Ham])"
putStrLn ""
putStrLn (show (noMeat (Sandwich [Meat Ham])))
putStrLn ""
putStr "noMeat (Sandwich [Bread Rye])"
putStrLn ""
putStrLn (show (noMeat (Sandwich [Bread Rye])))
putStrLn ""
putStr "noMeat (Sandwich [Bread Rye, Cheese Jack])"
putStrLn ""
putStrLn (show (noMeat (Sandwich [Bread Rye, Cheese Jack])))
putStrLn "" 
putStr "noMeat (Sandwich [Bread Rye, Cheese Jack, Meat Ham])"
putStrLn ""
putStrLn (show (noMeat (Sandwich [Bread Rye, Cheese Jack, Meat Ham])))
putStrLn ""
putStrLn "-----------------------------------------------------" 

putStrLn "4 inOSO"
putStrLn ""
putStr "inOSO (Sandwich [Meat Ham])"
putStrLn ""
putStrLn (show (inOSO (Sandwich [Meat Ham])))
putStrLn ""
putStr "inOSO (Sandwich [Bread Rye])"
putStrLn ""
putStrLn (show (inOSO (Sandwich [Bread Rye])))
putStrLn ""
putStr "inOSO (Sandwich [Bread Rye, Cheese Jack, Bread White, Bread Rye])"
putStrLn ""
putStrLn (show (inOSO (Sandwich [Bread Rye, Cheese Jack,Bread White, Bread Rye])))
putStrLn "" 
putStr "inOSO (Sandwich [Bread Rye, Cheese Jack, Meat Ham, Bread Rye])"
putStrLn ""
putStrLn (show (inOSO (Sandwich [Bread Rye, Cheese Jack, Meat Ham, Bread Rye])))
putStrLn ""
putStrLn "-----------------------------------------------------"  

putStrLn "4 intoOSO"
putStrLn ""
putStr "intoOSO (Sandwich [Meat Ham]) Rye"
putStrLn ""
putStrLn (show (intoOSO (Sandwich [Meat Ham]) Rye))
putStrLn ""
putStr "intoOSO (Sandwich [Bread Rye] White)"
putStrLn ""
putStrLn (show (intoOSO (Sandwich [Bread Rye]) White))
putStrLn ""
putStr "intoOSO (Sandwich [Bread Rye, Cheese Jack, Bread White, Bread Rye] White)"
putStrLn ""
putStrLn (show (intoOSO (Sandwich [Bread Rye, Cheese Jack,Bread White, Bread Rye]) White))
putStrLn "" 
putStr "intoOSO (Sandwich [Bread Rye, Cheese Jack, Meat Ham, Bread Rye])"
putStrLn ""
putStrLn (show (intoOSO (Sandwich [Bread Rye, Cheese Jack, Meat Ham, Bread Rye]) White))
putStrLn ""
putStrLn "-----------------------------------------------------" 

putStrLn "5 priceSandwich"
putStrLn ""
putStr "priceSandwich prices 0 (Sandwich [Meat Ham])"
putStrLn ""
putStrLn (show (priceSandwich prices 0 (Sandwich [Meat Ham])))
putStrLn ""
putStr "priceSandwich prices10 (Sandwich [Bread Rye])"
putStrLn ""
putStrLn (show (priceSandwich prices 10 (Sandwich [Bread Rye])))
putStrLn "" 
putStr "priceSandwich prices 0 (Sandwich [Bread Rye, Cheese Jack])"
putStrLn ""
putStrLn (show (priceSandwich prices 0 (Sandwich [Bread Rye, Cheese Jack])))
putStrLn ""
putStr "priceSandwich prices 5 (Sandwich [Bread Rye, Cheese Jack, Meat Ham])"
putStrLn ""
putStrLn (show (priceSandwich prices 5 (Sandwich [Bread Rye, Cheese Jack, Meat Ham])))
putStrLn "-----------------------------------------------------" 

putStrLn "6 eqSandwich"
putStrLn ""
putStr "eqSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])"
putStrLn ""
putStrLn (show (eqSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])))
putStrLn ""
putStr "eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
putStrLn ""
putStrLn (show (eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
putStrLn "" 
putStr "eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
putStrLn ""
putStrLn (show (eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
putStrLn ""
putStrLn "-----------------------------------------------------" 

putStrLn "7 equalSandwich"
putStrLn ""
putStr "equalSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])"
putStrLn ""
putStrLn (show (equalSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])))
putStrLn ""
putStr "equalSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
putStrLn ""
putStrLn (show (equalSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
putStrLn "" 
putStr "equalSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
putStrLn ""
putStrLn (show (equalSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
putStrLn ""
putStrLn "-----------------------------------------------------" 
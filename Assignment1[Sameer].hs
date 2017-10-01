import System.Random
import System.IO.Unsafe 

{-1. Develop a Haskell function sumsqbig that takes three numbers as arguments and 
		returns the sum of the squares of the two larger numbers. -}
sumsbig x y z = (x^2+y^2+z^2) - ((minimum[x,y,z])^2)

{-3. Develop a Haskell Boolean function div23n5 such that div23n5 n returns True 
	if and only if n is divisible by 2 or divisible by 3 but not divisible by 5. -}
div23n5 :: Integer->Bool
div23n5 n =
		if ((n `mod` 2==0 || n `mod` 3==0) && not(n `mod` 5==0)) then
			True
		else
			False

{-4. Develop a Haskell function notDiv such that notDiv n d returns True 
	if and only if integer n is not divisible by d. -}			
notDiv :: Integer -> Integer -> Bool
notDiv n d = 	if (not(n `mod` d==0 )) then
					True
				else
					False
					
{-5. Develop a Haskell function mult that takes two natural numbers (i.e., nonnegative integers) 
and returns their product. The function must not use the multiplication (*) or division (div) operators. 
Hint: Multiplication can be done by repeated addition-}
mult :: Integer -> Integer -> Integer
mult n m 
	|(n==0 || m==0)= 0
	|(m>0 && n>0)= n + mult n (m-1)
	| otherwise = error "something"

{-6. Develop a Haskell function addTax that takes two Double values such that 
	addTax c p returns c with a sales tax of p percent added-}
addTax ::Double-> Double-> Double
addTax c p = c+p*c/100

{-7. The time of day can be represented by a tuple (hours,minutes,m) 
	where m indicates either "AM" or "PM".
	Develop a Boolean Haskell function comesBefore that takes two time-of-day tuples and 
	determines whether the first is an earlier time than the second -}
--comesBefore ::(Integer->Integer->Char)->(Integer->Integer->Char)->Bool
comesBefore (hours,minutes,m) (hours2,minutes2,m2) = do
	let x
		|m=="PM" = 12
		|m=="AM" = 0
	let x2
		|m2=="PM" = 12
		|m2 =="AM" =0
	
	if (hours+x+(minutes)/60<hours2+x2+(minutes2)/60) then
		print("Before")
	else 	
		print("After")
		
{- 8. Develop a Haskell function
	minf :: (Int -> Int) -> Int
	such that minf g returns the smallest integer m such that 0 <= m <= 10000000
	and g m == 0 (if such an integer exists).
-}
minf :: (Int -> Int) -> Int
minf g 
	|funct valuef== 0 =g valuef  
	|otherwise =  g (valuef+1)


valuef :: Int
valuef = unsafePerformIO (getStdRandom (randomR (0, 10000000)))	

funct :: Int -> Int
funct n | n <=5000000= 0
funct n            = n
	
  

g :: Int -> Int
g p
  | (funct p==0) = g (p+1)
  | otherwise = funct p



{- Comments on 8. 
	valuef generates a random number to start the function. 
	funct  checks if the value is 5 million or not and gives the smallest number. It can be any functiona
	g checks if the value is smallest or not
	minf runs the function g untill the smallest value is reached and itterate it 
	import System.Random and import System.IO.Unsafe were imported to get random number as int form
-}




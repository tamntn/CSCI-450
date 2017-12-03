{-  CSci 450: Organization of Programming Languages
    Expression and Imperative Core Languages, Values
    Fall 2017 
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-19: Developed for both Expression, Imperative Core languages
2017-09-20: Added toNumType to handle conversion of string to NumType,
            including detecting fixed integer overflow

The intention of this module is to encapsulate definitions and
functions that know the specific representation of data of type
"ValType". Other modules should use the functions here to enable the
representation to be changed easily,

The only type of values supported is integer. The type ValType should
always be used to represent this type.

Boolean values are represented as integers where 0 represents False
and 1 represents True. For some purposes, any nonzero value
represents True.

-}

module Values
    ( NumType, ValType, Name, defaultVal, toNumType,
      falseVal, trueVal, boolToVal, valToBool
    )
where

type Name    = String
type NumType = Int
type ValType = NumType

-- default value for ValType entities (such as variables)
defaultVal :: ValType
defaultVal = 0


{-  Function "toNumType" converts unsigned integer strings to NumType
    values. If that can be reasonably done, it returns the value
    wrapped in a Right. Otherwise, it returns an error message wrapped
    in a Left.

    This version is specific to NumType as Int.
-}

toNumType :: String -> Either String NumType
toNumType num =
    case (read num) :: Integer of
        x | x > maxInt -> Left (num ++ " exceeds Int range!")
        x              -> Right ((fromInteger x) :: Int)

maxInt = toInteger (maxBound :: Int)


{-  Constants "falseVal" and "trueVal" and functions "boolToVal"
    and valToBool encapsulate the representation of Boolean
    values as ImpCore values. 

    Function "boolToVal" takes a Haskell Bool value and returns
    the equivalent ImpCore encoding "falseVal" or "trueVal".

    Function "valToBool" takes a ValType value and returns the
    equivalent Haskell Bool value.

    For ImpCore, any nonzero integer value is considered as "truthy"
    in some circumstances. Only zero is consider as "falsey".
-}

falseVal, trueVal :: ValType
falseVal = 0
trueVal  = 1

boolToVal :: Bool -> ValType
boolToVal True  = trueVal
boolToVal False = falseVal

valToBool :: ValType -> Bool
valToBool x = (x /= falseVal)

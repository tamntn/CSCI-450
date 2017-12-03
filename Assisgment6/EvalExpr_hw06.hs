{-  CSci 450: Organization of Programming Languages
    Expression Language, Evaluator ASSIGN #6 Mods
    Fall 2017 
    H. Conrad Cunningham

12345678901234567890123456Env78901234567890123456789012345678901234567890

2017-04-03: Version based partly on previous work in Scala & Lua
2017-04-07: Updated with type aliases to match case study document
2017-05-23: Updated to match Prefix Parser V5
2017-06-02: Modified to include underscore as first char of
            identifier
2017-08-14: Prototype based on previous ExprLang and ImpCore work
2017-09-19: Separated out AbSynEval, Environments, and ProcessAST.
            Used similar approach to ImpCore modularization.
            Allow for error returns from eval.
2017-11-04: Repaired bug in setNameBinding
2017-11-05: Modified imported items, improved comments
2017-11-10: Modified for Assignment #6

-}

module EvalExpr_hw06
    ( ValType, Name, Expr(..), Env, EvalErr, eval, lastVal,
      newEnviron, showEnviron, getNameBinding, hasNameBinding,
      newNameBinding, setNameBinding
    )
where

-- Haskell libraries

-- Expression Language modules
import Values       ( ValType, Name, defaultVal, boolToVal, valToBool ) -- more items later
-- import AbSynExpr   ( Expr(..) ) -- uses ValType, Name
import AbSynExpr_hw06 ( Expr(..) )
import Environments ( AnEnv, newEnv, toList, getBinding, -- uses Name
                      hasBinding, newBinding, setBinding, bindList )

-- Data types
type EvalErr = String
type Env     = AnEnv ValType


{-  Functions to create and update the environment used by the
    evaluator module and its clients. They use the generic
    functionality provided by the Environment module.
-}

{- Variable lastVal holds the value of the most recent previous
   expression.
-}

lastVal :: Name
lastVal = "it"

-- Create a new environment with only lastVal set to default value
newEnviron :: Env
newEnviron = newBinding lastVal defaultVal newEnv

-- Get the current binding of a name wrappted in a Maybe
getNameBinding :: Name -> Env -> Maybe ValType
getNameBinding n env = getBinding n env

-- Is the name currently bound to a value in the environment?
hasNameBinding :: Name -> Env -> Bool
hasNameBinding n env = hasBinding n env

{- Create new variable n with the initial value v,
   assuming the variable does not previously exist.
-}

newNameBinding :: Name -> ValType -> Env -> Env
newNameBinding n v env = newBinding n v env

{- Set existing variable n to a new value v, assuming the variable
   exists.
-}

setNameBinding :: Name -> ValType -> Env -> Env
setNameBinding n v env = setBinding n v env


{-  Function "showEnviron" takes an environment and returns a string
    of its contents suitable for display.
-}

showEnviron:: Env -> String
showEnviron env =
    concatMap
        (\(n,v) -> n ++ "\t" ++ show v ++ "\n")
        (toList env)


-- Evaluate an expression tree
eval :: Expr -> Env -> Either EvalErr ValType

eval (Val v) _   = Right v

eval (Var n) env = 
    case getBinding n env of
        Nothing -> Left ("Undefined variable " ++ n)
        Just i  -> Right i

eval (Add l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv + rv)
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Sub l r) env = 
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv - rv)
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Mul l r) env = 
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv * rv)
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Div l r) env =
    case (eval l env, eval r env) of
        (Right _,  Right 0 ) -> Left "Division by 0"
        (Right lv, Right rv) -> Right (lv `div` rv)
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

-- 1. Extend the abstract syntax tree data type Expr (in the Abstract Syntax module) 
-- to add new operations Neg (negation), Min (minimum), Max (maximum), and Exp (exponentiation).
eval (Neg e) env = 
    case (eval e env) of
        (Right lv) -> Right (-lv)
        (Left le) -> Left (le)

eval (Min l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv`min`rv)
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Max l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv `max` rv) 
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Exp l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (lv ^ rv) 
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

-- 4. Extend the abstract syntax tree data type Expr to include the binary operators Eq (equality) 
-- and Lt (less-than comparison), logical unary operator Not, and the ternary conditional expression If (if-then-else).
eval (Eq l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (boolToVal (lv == rv)) 
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Lt l r) env =
    case (eval l env, eval r env) of
        (Right lv, Right rv) -> Right (boolToVal (lv < rv)) 
        (Left le,  Left re ) -> Left (le ++ "\n" ++ re)
        (x@(Left le),  _   ) -> x
        (_,     y@(Left le)) -> y

eval (Not l) env =
    case (eval l env) of
        (Right lv) -> Right (boolToVal (not(lv==0))) 
        (Left le ) -> Left (le)

eval (If c l r) env =
    case (eval c env, eval l env, eval r env) of
        (Right cv, Right lv, Right rv) 
            | not(cv==0) -> Right (lv)
            | otherwise -> Right (rv) 
        (Left ce, Left le, Left re) -> Left (le ++ "\n" ++ re)
        (x@(Left le), _, _) -> x
        (_, y@(Left le), _) -> y
        (_, _, z@(Left le)) -> z
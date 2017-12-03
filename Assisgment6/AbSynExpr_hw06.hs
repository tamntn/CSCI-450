{-  CSci 450: Organization of Programming Languages
    Expression Language, Abstract Syntax
 ASSIGN #6 Mods
    Fall 2017 
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-19: New module separated from PrefixParser06 with changes
            similar to current AbSynImpCore module
2017-09-20: Corrected show functionality for "*" (kam)
2017-11-10: Modified for Assignment #6

The intention of this module is to encapsulate the abstract syntqx as
much as is practical -- such as to centralize the data type
definitions and the Show functionality.

However, given that the abstract syntax consists of algebraic data
type definitions, the semantics of the abstract syntax tree is known
by modules that must create (e.g., parser) and use (e.g., evaluator)
the abstract syntax trees.

TODO:

- Decide whether to use explicit Show instance or default given that
  this will be used with two concrete syntaxes

-}

module AbSynExpr_hw06
    ( ValType, Name, Expr(..) )
where

-- Haskell libraries
import Data.List ( intercalate )

-- Imperative Core modules
import Values ( ValType, Name )

{- ABSTRACT SYNTAX TREE (AST) -}

data Expr = Add Expr Expr 
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Name
          | Val ValType 
          | Neg Expr           -- added
          | Min Expr Expr      -- added
          | Max Expr Expr      -- added
          | Exp Expr Expr      -- added
          | Eq  Expr Expr      -- added
          | Lt  Expr Expr      -- added
          | Not Expr           -- added
          | If  Expr Expr Expr -- added


-- Define Show instance for Expr to enable convenient string format
instance Show Expr where
    show (Val v)   = show v
    show (Var n)   = n
    show (Add l r) = showParExpr "+"   [l,r]
    show (Sub l r) = showParExpr "-"   [l,r]
    show (Mul l r) = showParExpr "*"   [l,r]
    show (Div l r) = showParExpr "/"   [l,r]
    show (Neg v)   = showParExpr "neg" [v]
    show (Min l r) = showParExpr "min" [l,r]
    show (Max l r) = showParExpr "max" [l,r]
    show (Exp l r) = showParExpr "^"   [l,r]
    show (Eq l r)  = showParExpr "=="  [l,r]
    show (Lt l r)  = showParExpr "<"   [l,r]
    show (Not v)   = showParExpr "not" [v]
    show (If c l r) = showParExpr "if" [c,l,r]

showParExpr :: String -> [Expr] -> String
showParExpr op es = "(" ++ op ++ " " ++ showExprList es ++ ")"
    
showExprList :: [Expr] -> String
showExprList es = intercalate " " (map show es)


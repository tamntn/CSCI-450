{-  CSci 450: Organization of Programming Languages (Revised 11/28)
    Prefix Expression Language: Rec. Descent Parser ASSIGN #6 Mods
    Fall 2017 
    H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2007-05-10: Developed a prefix interpreter in a homework solution
2017-04-05: V1--Developed prefix parser from 2017-04-05 (V1)
            infix recognizer and the earlier prefix interpreter code
2017-04-06: V2--Generalized collection of an operator's operands,
            added extended operations
2017-04-07: V3--Added support for multicharacter operators,
            separated lexical analysis into lexer and lexx,
            improved error handling
2017-05-20: V4--Separated lexical analysis & testing into modules,
            removed "validExpr", added "parse"
2017-05-23: V5--Replaced use of BadChar Expr with Either,
            renamed nestExpr as parseOpExpr & gatherOperands as
            parseOperands for better explanation of parsing,
            simplified parseOpExpr & parseOperands using cases,
            improved comments
2017-06-12: V6--Updated parser for grammar change & recursive descent
            explanations in course notes; renamed parseExpr to
            parseExpression, parseOpExpr to parseOperExpr,
            parseOperands to parseOperandSeq
2017-07-06: Comment updates to V6
2017-09-20: Separated AbSynExpr module. Modified to be similar to
            ParseImpCore. Made error-handling similar to ParseInfix03.
            Added getName and getValue. Renamed to ParsePrefixExpr
2017-11-05: Improved comments
2017-11-10: Modified for Assignment #6
2017-11-28: Fixed typo related to handling of "max" operator

This module implements a simple recursive descent parser for the
following grammar:

    <expression> ::=  <var> | <val> | <operexpr>
    <var>        ::=  <id>
    <val>        ::=  [ "-" ] <unsigned> 
    <operexpr>   ::=  "(" <operator> <operandseq> ")"
    <operandseq> ::=  { <expression> }
    <operator>   ::=  '+'  |  '*'  |  '-'  |  '/' | ...
  
As long as the lexical characteristics do not change, it should be
relatively easy to extend this module to support additional operators.

TODO:
- Perhaps make error handling similar to ParseInfixExpr

-}

module ParsePrefixExpr_hw06
    ( ValType, Name, ParErr, Expr(..), parse, parseExpression,
      trimComment, getName, getValue
    )
where

-- Haskell libraries
import Data.Maybe ( fromMaybe )
import Data.Char  ( isSpace, isAlpha, isAlphaNum, isDigit )

-- Expression Language modules
import Values         ( NumType, ValType, Name )
import AbSynExpr_hw06 ( Expr(..) )  -- uses ValType, Name
import LexExpr_hw06   ( Token(..), lexer, showTokens )
                      -- uses NumType, Name

-- Type definitions
type ParErr = String

-- Maximum prefix of the token sequence displayed for errors
errpref = 10

-- Trim a prefix of the Token list
pref xs = take errpref xs


{- PARSING -}

{-  Function "trimComment" trims an end-of-line comment from a line
    of text input. End-of-line comments begin with "beginComment".
-}

beginComment = ';'

trimComment :: String -> String
trimComment = takeWhile (/= beginComment)


{-  Function "getName" takes a string and returns a Just
    wrapping a Name if it is a valid identifier or a Nothing
    if any non-identifier characters occur.
-}

getName :: String -> Maybe Name
getName xs = 
    case getId xs of
        (xs@(_:_),[]) -> Just xs
        otherwise     -> Nothing


{-  Function "getId" extracts an identifer from the beginning
    of a string and returns the identifier and the remaining string.
-}

getId :: String -> (Name,String)
getId []          = ([],[])
getId xs@(x:_)
    | isFirstId x = span isRestId xs
    | otherwise   = ([],xs)
    where
        isFirstId c = isAlpha c    || c == '_'
        isRestId  c = isAlphaNum c || c == '_'


{-  Function "getValue"takes a string and returns a Just wrapping
    the value if it is a valid value literal and returns a
    Nothing if the argument is not a valid literal.
-}

getValue :: String -> Maybe ValType
getValue xs =
    case getInt xs of
        (xs@(_:_),[]) -> Just (read xs)
        otherwise     -> Nothing


{-  Function "getInt" takes a string and extracts an integer
    literal from the beginning of a string and returns the integer
    literal and the remaining string.
-}

getInt :: String -> (String,String)
getInt xs@(x:xs')
    | isDigit x = span isDigit xs
    | x == '+'  = span isDigit xs'
    | x == '-'  = let (ys,zs) = span isDigit xs'
                  in  (x:ys,zs)
    | otherwise = ([],xs)


{-  FULL GRAMMAR PARSING  -}

{- Function "parse" takes an input expression, processes it with the
   lexical analyzer "lexer" and the recursive descent prefix parser
   "parseExpression", and returns an Either item wrapping the Expr
   abstract syntax tree or an error message. There must not be extra
   nonspace charaters at the end of the input.
-}

parse :: String -> Either ParErr Expr
parse xs =
    case lexer xs of
        [] -> incompleteExpr xs
        ts ->
            case parseExpression ts of
               (ex@(Right _), []) -> ex
               (ex@(Left  _), _ ) -> ex
               (ex, ss)           -> extraAtEnd ex ss

incompleteExpr xs =
    Left ("Incomplete expression: " ++ xs)

extraAtEnd ex xs =
   Left ("Nonspace token(s) \"" ++ (showTokens xs) ++ 
         "\" at end of the expression \"" ++ (show ex) ++ "\"")


{- Function "parseExpression" takes a Token list, parses an Expr, and
   returns a pair consisting of an Either wrapping the Expr abstract
   syntax tree found and the list of Tokens remaining after the Expr.
   An error is denoted by returning the Left value for the Either.

   Function "parseExpression" implements the following BNF rule:

        <expression> ::=  <var> | <val> | <operexpr> 
-}

parseExpression :: [Token] -> (Either ParErr Expr, [Token])
parseExpression xs =
    case parseVar xs of
        r@(Right _, _) -> r  -- <var>
        _ ->
          case parseVal xs of
              r@(Right _, _) -> r  -- <val>
              _ ->
                  case parseOperExpr xs of
                      r@(Right _, _) -> r  -- <operexpr>
                      (Left m, ts)  -> (missingExpr m ts, ts)  

missingExpr m ts  =
    Left ("Missing expression at  " ++ (showTokens (pref ts))
           ++ "..\n..Nested error { " ++ m ++ " }")


{- Function "parseVar" takes a Token list, parses a <var>, and
   returns a pair consisting of an Either wrapping the <var> abstract
   syntax tree found and the list of Tokens remaining after the <var>.
   An error is denoted by returning the Left value for the Either.

   Function "parseVar" implements the following BNF rule, where <id>
   is a lexical Token:

       <var> ::= <id>
-}
        
parseVar :: [Token] -> (Either ParErr Expr, [Token])
parseVar ((TokId id):ts) = (Right (Var id),ts)
parseVar ts              = (missingVar ts, ts)

missingVar ts =
    Left ("Missing variable at " ++ (showTokens (pref ts)))

                    
{- Function "parseVal" takes a Token list, parses an unsigned <val>,
   and returns a pair consisting of an Either wrapping the <val>
   abstract syntax tree found and the list of Tokens remaining after
   the <val>.  An error is denoted by returning the Left value for the
   Either.

   Function "parseVal" implements the following BNF rule:

       <val> ::= [ "-" ] <unsigned>
-}
        
parseVal :: [Token] -> (Either ParErr Expr, [Token])
parseVal ((TokNum i):ts)             = (Right (Val i), ts)
parseVal ((TokOp "-"):(TokNum i):ts) = (Right (Val (-i)), ts)
parseVal ts                          = (missingVal ts, ts)

missingVal ts =
    Left ("Missing value at " ++ (showTokens (pref ts)))


{- Function "parseOperExpr" takes a token list, parses an operator
   expression enclosed in parentheses, and returns a pair consisting
   of an Either wrapping the Expr abstract syntax tree found and
   the list of Tokens remaining after the Expr. An error is denoted
   by returning the Left value for the Either. This function
   implements the following BNF rule:

        <operexpr>   ::=  "(" <operator> <operandseq> ")"

   This function examines the two tokens to see if they are a left
   parenthesis and an operator rather than calling separate function.
   Similarly, it examines whether the last token is a right
   parenthesis.

   The operator has to be handled differently because it is not
   a complete expression and takes differing numbers of operands.
-}

parseOperExpr :: [Token] -> (Either ParErr Expr, [Token])
parseOperExpr xs@(TokLeft:(TokOp op):ys) =  -- "(" <operator>
    case parseOperandSeq ys of              -- <operandseq>
        (args, zs) ->
            case zs of                      -- ")"
              (TokRight:zs') -> (makeExpr op args, zs')
              zs'            -> (missingRParen zs, xs)
-- ill-formed <operexpr>s
parseOperExpr (TokLeft:ts)      = (missingOp ts, ts)
parseOperExpr (TokRight:ts)     = (invalidOpExpr ")", ts)
parseOperExpr ((TokOther s):ts) = (invalidOpExpr s, ts)
parseOperExpr ((TokOp op):ts)   = (invalidOpExpr op, ts)
parseOperExpr ((TokId s):ts)    = (invalidOpExpr s, ts)
parseOperExpr ((TokNum i):ts)   = (invalidOpExpr (show i), ts)
parseOperExpr []                = (incompleteExpr2, [])

missingRParen ts =
    Left ("Missing `)` at " ++ (showTokens (pref ts)))
missingOp ts =
    Left ("Missing operator at " ++ (showTokens (pref ts)))
invalidOpExpr s =
    Left ("Invalid operation expression beginning with " ++ s)
incompleteExpr2 =
    Left "Incomplete expression"


{- Function "parseOperandSeq" takes a token list and collects a list
   of 0 or more operands. An empty list means that no operands were
   found. It implements the following BNF rule:

        <operandseq> ::=  { <expression> }

   The output differs from the other parse functions in that it
   returns a (possibly empty) list of expressions rather than an
   Either wrapping an expression.

-}

parseOperandSeq :: [Token] -> ([Expr],[Token])
parseOperandSeq xs =
    case parseExpression xs of 
        (Left  _,      _ ) -> ([],xs)
        (Right ex, ys) ->
            let (exs,zs) = parseOperandSeq ys
            in  (ex:exs,zs)


{- AST CONSTRUCTION -}

{- Function "makeExpr" takes the operator string and a list of
   operand expressions and constructs an appropirate Expr.
-}

makeExpr :: String -> [Expr] -> Either ParErr Expr
makeExpr op exs =
    case arity op of
        0 -> opCons0 op exs
        1 -> opCons1 op exs
        2 -> opCons2 op exs
        3 -> opCons3 op exs
        4 -> opCons4 op exs
        5 -> opCons5 op exs
        _ -> opConsX op exs


{-  Function "arity" takes an operator symbol and returns the
    number of operands >= 0 that operator requires or returns
    -1 for items not in the arityMap association list.
-}

-- arityMap = [ ("+",2), ("-",2), ("*",2), ("/",2) ]
           -- add (operator,arity) pairs as needed
arityMap = [ ("+",2),   ("-",2),   ("*",2),   ("/",2),
             ("neg",1), ("min",2), ("max",2), ("^",2),
             ("==",2),  ("<",2),   ("not",1), ("if",3) 
           ]
arity :: String -> Int
arity op = fromMaybe (-1) (lookup op arityMap)


{- Function "opCons1" takes a unary operator string and an operand
   list with one element and returns the corresponding Expr structure
   wrapped in a Right. An error is denoted by passing back a Left.
-}

-- assocOpCons1 = [ ] -- add (operator_string,Constructor) pairs
--                   -- as needed, e.g., like ("sqrt",Sqrt)
assocOpCons1 = [ ("neg",Neg), ("!",Not) ]

opCons1 :: String -> [Expr] -> Either ParErr Expr
opCons1 op exs =
    case length exs of
        1 -> case lookup op assocOpCons1 of
                Just c  -> Right (c (exs!!0))
                Nothing -> invalidOp op
        n -> arityErr op n


{- Function "opCons2" takes a binary operator string and an operand
   list with two elements and returns the corresponding Expr
   structure wrapped in a Right. An error is denoted by passing back
   a Left.
-}

-- assocOpCons2 =
--  [ ("+",Add), ("-",Sub), ("*",Mul), ("/",Div) ]
     -- add new pairs as needed
assocOpCons2 =
    [ ("+",Add),   ("-",Sub),   ("*",Mul), ("/",Div),
      ("min",Min), ("max",Max), ("^",Exp), ("==",Eq), ("<",Lt) ]
  
opCons2 :: String -> [Expr] -> Either ParErr Expr
opCons2 op exs =
    case length exs of
        2 -> case lookup op assocOpCons2 of
                Just c  -> Right (c (exs!!0) (exs!!1))
                Nothing -> invalidOp op
        n -> arityErr op n


{- Function "opCons3" takes a ternary operator string and an operand
   list with three elements and returns the corresponding Expr
   structure wrapped in a Right. An error is denoted by passing back a
   Left.
-}

-- assocOpCons3 = [ ] -- add new pairs as needed
assocOpCons3 = [ ("if",If) ] 

opCons3 :: String -> [Expr] -> Either ParErr Expr
opCons3 op exs =
    case length exs of
        3 -> case lookup op assocOpCons3 of
                Just c  -> Right (c (exs!!0) (exs!!1) (exs!!2))
                Nothing -> invalidOp op
        n -> arityErr op n

-- For future use

opCons0, opCons4, opCons5, opConsX ::
    String -> [Expr] -> Either ParErr  Expr
opCons0 op exs = unsupportedOp op
opCons4 op exs = unsupportedOp op
opCons5 op exs = unsupportedOp op
opConsX op exs = unsupportedOp op

-- Error message for AST Construction functions

invalidOp op =
    Left ("Invalid operator '" ++ op ++ "'")
arityErr op n =
    Left ("Operator '" ++ op ++ "' incorrectly called with " ++
          (show n) ++ " operand(s)")
unsupportedOp op =
    Left ("Unsupported operator '" ++ op ++ "'")

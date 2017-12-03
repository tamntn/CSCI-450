{- CSci 450: Organization of Programming Languages
   Expression Language, Lexical Analyzer ASSIGN #6 Mods
   Fall 2017 
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-05-23: Separated lexical analyzer from Prefix Parser V3,
            adapted xformTok to support both prefix and infix
            languages
2017-06-13: Modified to accept underscore as first Id character,
            Added colon and backslash to operator symbols for future,
            Updated grammar and other comments
2017-09-19: Modularized and updated to be more similar to the lexical
            analyzer for ImpCore. Added comment removal.
            Moved showTokens from infix parser.
2017-09-20: Added convertNumType to handle integer overflow
2017-11-10: Modified for Assignment #6

Function "lexx" does not distinguish among keywords (identifier-like
primitive function names) and user-defined variable names. All are
identifiers.

Function "lexer" uses "lexx" but then moves keywords into the operatorcategory and unsupported operators into the other category.

-}


module LexExpr_hw06
    ( NumType, Name, Token(..), showTokens, lexx, lexer )
where

-- Haskell libraries
import Data.Char ( isSpace, isDigit, isAlpha, isAlphaNum )

-- ExprLang modules
import Values ( NumType, Name, toNumType )

-- Token algebraic data type
data Token = TokLeft          -- left parenthesis
           | TokRight         -- right parenthesis
           | TokNum NumType   -- unsigned integer literal
           | TokId Name       -- names of variables, etc.
           | TokOp Name       -- names of primitive functions
           | TokKey Name      -- keywords
           | TokOther String  -- other characters
             deriving (Show, Eq)

-- Convert a Token list to a corresponding String
showTokens :: [Token] -> String
showTokens []                = ""
showTokens ((TokId s):xs)    = s ++ ' ' : showTokens xs
showTokens ((TokNum v):xs)   = (show v) ++ ' ' : showTokens xs
showTokens ((TokOp s):xs)    = s ++ ' ' : showTokens xs
showTokens (TokLeft:xs)      = '(' : ' ' : showTokens xs
showTokens (TokRight:xs)     = ')' : ' ' : showTokens xs
showTokens ((TokOther s):xs) = s ++ ' ' : showTokens xs


{-  Function "lexx" takes a string and returns the corresponding list
    of lexical tokens. It uses a regular grammar to group characters
    into parenthesis characters, unsigned integers, identifiers, and
    operators.  It skips space characters and end-of-line comments
    (after ';') and places any unexpected characters into an "other"
    token. It captures <id> and <unsigneds> to be as long as possible

        <input>    ::=  <token>  |  <token> <input>
        <token>    ::=  '('   |  ')'   |  <unsigned>
                     |  <id>  |  <op>  |  <other>
        <unsigned> ::=  <digit>  |  <digit> <unsigned> 
        <digit>    ::=  any numeric character
        <id>       ::=  <firstid>  |  <firstid> <idseq>
        <idseq>    ::=  <restid>   |  <restid> <idseq>
        <firstid>  ::=  <alpha>  | '_'
        <restid>   ::=  <alpha>  | '_'  | <digit>
        <op>       ::=  '+'  |  '*'  |  '-'  |  '/'  | ...
        <digit>    ::=  any numeric character
        <alpha>    ::=  any alphabetic character 
        <other>    ::=  any other character

-}

lexx :: String -> [Token]
lexx []  = []
lexx xs@(x:xs')
    | isSpace x   = lexx xs'
    | x == ';'    = lexx (dropWhile (/='\n') xs')
    | x == '('    = TokLeft  : lexx xs'
    | x == ')'    = TokRight : lexx xs'
    | isDigit x   = let (num,rest) = span isDigit xs
                    in (TokNum (convertNumType num)) : lexx rest
    | isFirstId x = let (id,rest) = span isRestId xs
                    in  (TokId id) : lexx rest
    | isOpChar x  = let (op,rest) = span isOpChar xs
                    in (TokOp op) : lexx rest
    | otherwise   = (TokOther [x]) : lexx xs'
    where 
        isFirstId c = isAlpha c    || c == '_' 
        isRestId  c = isAlphaNum c || c == '_' 
        isOpChar  c = elem c opchars 

-- Characters allowed in opertor strings
opchars = "+-*/~<=>!&|@#$%^?:"  -- not " ' ` ( ) [ ] { } , . ;


{-  Function "convertNumType" converts and unsigned number string
    into a NumType value.  If that cannot be done, it does an
    error call.

-}

convertNumType :: String -> NumType
convertNumType num  =
    case toNumType num of
        Right v  -> v
        Left err -> error ("Lexical error: " ++ err)


{-  Function "lexer" takes a string and returns the corresponding list
    of lexical tokens. 

    It uses "lexx" to tokenize the input and then uses "markSpecials"
    to transform (a) identifier tokens corresponding to keywords into
    operator tokens and (b) operator tokens that do not correspond to
    supported operators into Other tokens.
-}

lexer :: String -> [Token]
lexer xs = markSpecials (lexx xs)

markSpecials :: [Token] -> [Token]
markSpecials ts = map xformTok ts

xformTok :: Token -> Token
xformTok t@(TokId id)
    | elem id keywords   = TokOp id
    | otherwise          = t
xformTok t@(TokOp op)
    | elem op primitives = t
    | otherwise          = TokOther op
xformTok t               = t

-- Keywords (identifiers that are reserved as operators) and
-- primitives (operator strings that are supported)
keywords, primitives :: [Name]
-- keywords   = []                -- add strings for keywords
-- primitives = ["+","-","*","/"] -- add strings for operators
keywords   = ["neg","min","max","if"]           
primitives = ["+","-","*","/","==","<","^","!"] 

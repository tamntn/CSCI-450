{- CSci 450: Organization of Programming Languages
   Expression Language, Prefix Syntax, REPL ASSIGN #6 Mods
   Fall 2017 
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-20: Adapted from ImpCore REPL
2017-11-05: Modified imports, improved comments
2017-11-10: Modified for Assignment #6

-}

module ExprPrefixREPL
where

-- Haskell libraries
import Data.List  ( dropWhileEnd, intercalate )
import Data.Char  ( isSpace )
import Data.Maybe ( fromMaybe )
import System.Console.Haskeline  -- leave open for now
import Control.Monad.IO.Class ( liftIO )

-- Expression Language modules
-- import ParsePrefixExpr
import ParsePrefixExpr_hw06
    ( ValType, Name, ParErr, Expr(..), parse,
      trimComment, getName, getValue )
-- import EvalExpr
import EvalExpr_hw06
    ( Env, EvalErr, newEnviron, showEnviron, lastVal, eval,
      getNameBinding, newNameBinding, setNameBinding )
      -- uses Expr(..), ValType, Name

{-  Function "main" is the entry point for the interpreter program.
    It uses the Haskeline library to do interactive input/output.
-}

helptext :: String
helptext = intercalate "\n"
    [ "Welcome to the PREFIX Expression Language REPL."
    , "You may enter or one of the commands"
    , "    :quit          to exit the interpreter"
    , "    :display       to display the bindings in the environment"
    , "    :set var int   to create a variable or changee its value"
    , "or any valid Expression Language prefix expression."
    ]
 
main :: IO ()
main = do
    putStrLn helptext
    runInputT defaultSettings (repl newEnviron)


{-  Function "repl" is the Read-Evaluate-Print-Loop for the
    interpreter. It reads expressions from the terminal, parses
    the input, evaluates the expressions in sequence, and prints
    the result.  It binds the value of the last expression

    In addition to the definitions/expressions of the language, it
    takes the following commands:
        :quit         -- terminates the interpreter loop
        :display      -- displays the environment
        :use filename -- includes the file contents at that point in
                         sequence of definitions
TODO:
-- Factor out common code to simplifiy deeply nested REPL

-}

-- Principal and subsidiary prompts for interactive interpreter
prompt         = "REPL>  "
continuePrompt = "> "

-- Interpreter loop
repl :: Env -> InputT IO ()
repl env = do
    line <- getInputLine prompt
    let cmd = trim $ fromMaybe "" line
 
    case words cmd of

        []               -> repl env

        [":quit"]        -> return ()

        [":display"]     -> do
            outputStrLn $ showEnviron env
            repl env

        [":set",id,newv] -> do
            case (getName id, getValue newv) of 
                (Nothing, _) -> do
                    outputStrLn $ "Invalid variable name " ++ id
                    repl env
                (_, Nothing) -> do
                    outputStrLn $ "Invalid value " ++ newv
                    repl env
                (Just id', Just newv') -> do
                    case getNameBinding id' env of
                        Nothing   -> do
                            let env' = newNameBinding id' newv' env
                            outputStrLn $
                                "New variable '" ++ id' ++
                                "' created with value " ++ show newv'
                            repl env'
                        Just oldv -> do
                            let env' = setNameBinding id' newv' env
                            outputStrLn $
                                "Value of variable '" ++ id'
                                ++ "' changed from " ++ show oldv
                                ++ " to " ++ show newv'
                            repl env'

        _            -> do  -- expression evaluation
            allLines <- getMultiline 0 cmd
            case allLines of
                Nothing -> do
                    outputStrLn $
                        "Unbalenced parentheses in definition near "
                        ++ cmd
                    repl env
                Just fullcmd -> do
                    let ex = parse fullcmd 
                    case ex of
                        Left err -> do
                           outputStrLn $ "Parse Error: " ++ err
                           repl env
                        Right theEx -> do
                            case eval theEx env of
                                Left err -> do
                                    outputStrLn $
                                        "Evaluation error " ++ err
                                    repl env
                                Right res -> do
                                    putEvalResult res
                                    let env' = setNameBinding lastVal
                                                   res env
                                    repl env'
                            

{-  Function "printEvalResult" takes the return the "evalScript"
    function and prints the result to the standard output.
-}

putEvalResult :: ValType -> InputT IO ()
putEvalResult v = outputStrLn $ "Value:    " ++ show v


{-  Function "getMultiline" takes the previous nesting level of
    parentheses (>= 0) and the next line of the input script. It reads
    additional lines, if needed, until the whole script ends with
    properly nested parentheses. It's return is a Just wrapping the
    concatenated input lines (minus comments and some unnecessary
    white space) or a Nothing if the number of left parentheses
    exceeds the number of right.

    It is intended for use with interactive I/O using Haskeline.
-}

getMultiline :: Int -> String -> InputT IO (Maybe String)
getMultiline n cmd1 =
    case checkParenBalance n cmd1 of
        Nothing         -> return Nothing         
        Just m | m == 0 -> return (Just cmd1)
        Just m | m > 0  -> do
            line2 <- getInputLine continuePrompt
            case line2 of
                Nothing   -> return (Just cmd1)
                Just cmd2 -> do
                    rest <- getMultiline m $ trim cmd2
                    return (Just (cmd1 ++ (fromMaybe "" rest)))
        Just m -> return Nothing -- should not occur


{-  Function "checkParenBalance" takes an integer denoting the
    nesting level of parentheses (>= 0) in prevous lines and the
    next line of text to check for balanced parentheses. It returns
    a Just wrapping the updated nesting level after the next line.
    At any point at which there are more left-parentheses than
    right, it returns a Nothing.
-}

checkParenBalance :: Int -> String -> Maybe Int
checkParenBalance n _ | n < 0 = Nothing
checkParenBalance n []        = Just n
checkParenBalance n (x:xs)
    | x == '('  = checkParenBalance (n+1) xs -- increase level
    | x == ')'  = checkParenBalance (n-1) xs -- decrease level
    | otherwise = checkParenBalance n xs     -- no change


{-  Function "trim" takes a line of text, removes any
    beginning-of-line white space, then any end-of-line comment,
    and then any end-of-line white space. It returns the remaining
    text with a newline charcter appended to the end.
-}

trim :: String -> String
trim xs = (trimTrailing . trimComment . trimLeading) xs  ++ "\n"

trimTrailing :: String -> String
trimTrailing = dropWhileEnd isSpace
  
trimLeading :: String -> String
trimLeading = dropWhile isSpace


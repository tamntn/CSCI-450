{- CSci 450: Organization of Programming Languages
   Expression and Imperative Core Language, Environments
   Fall 2017 
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-14: Separated out from Imperative Core evaluator code
2017-09-19: Modified slightly to work with both language interpreters
2017-11-02: Added missing type signature for hasBinding

The intention of this module is to encapsulate how environments are
implemented. From the perspective of users of the module, an
environment is a set of (Name,ValType) pairs.

This module implements an environment as a simple association list
mapping names to values.

As long as the same interface is supported, it should be possible to
replace the association list with a Map or other data structure.

-}

module Environments
    ( AnEnv, Name, newEnv, toList,
      getBinding, hasBinding, newBinding, setBinding, bindList
    )
where

-- Expression Language modules
import Values ( Name )

type AnEnv a = [(Name,a)]          -- generic environment type


{-  Function "newEnv" returns a new empty environment using the
    underlying environment representation.
-}

newEnv :: AnEnv a
newEnv = []


{-  Function "toList" takes an environment (AnEnv) and returns the
    environment as a list of name-value pairs.
-}

toList :: AnEnv a -> [(Name,a)]
toList xs = xs


{-  Function "getBinding" takes a name and an environment,
    "looks up" the name in the environment and returns the value
    bound to the name wrapped in a Just.  A return of Nothing means
    the name has no binding in the environment.
-}

getBinding :: Name -> AnEnv a -> Maybe a
getBinding n env = lookup n env


{-  Function "hasBinding" takes a name and an environment and
    determines whether or not a name is bound in the environment.
-}

hasBinding :: Name -> AnEnv a -> Bool
hasBinding n env =
    case getBinding n env of
        Just _  -> True
        Nothing -> False


{-  Function "newBinding" takes a name, a value, and an environment
    and returns a copy of the environment with the name bound to the
    value.

    This function is intended for use when there is no previous
    binding of the name in the environment.
-}

newBinding :: Name -> a -> AnEnv a -> AnEnv a
newBinding n v env = (n,v):env
   

{-  Function "setBinding" takes a name, a value, and an environment.
    If the name occurs in the environment, it returns a copy of the
    environment with the name bound to the input value. Otherwise,
    the functions throws an error condition.
-}

setBinding :: Name -> a -> AnEnv a -> AnEnv a
setBinding n v env =
    let (ls,rs) = break (\(k,_) -> k == n) env
        trs = if null rs then
                  error ("Attempt to set undefined variable " ++ n)
              else tail rs
    in  ls ++ ((n,v):trs)


{-  Function "bindList" takes a list of name-value pairs and an
    environment and returns an updated environment with these
    names and values bound.
-}

bindList :: [(Name,a)] -> AnEnv a -> AnEnv a
bindList nvs env =
    foldl (\env1 (n,v) -> newBinding n v env1) env nvs

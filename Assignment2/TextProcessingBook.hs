{-  CSci 450/502, Org. of Programming Languages, Fall 2017
    H. Conrad Cunningham, Assignment #2

1234567890123456789012345678901234567890123456789012345678901234567890

2000-02-15: Original version
2007-02-18: Add import of Haskell 98 Char module
2017-09-12: Update for Haskell 2010, put in module, use Data.Char

-}

module TextProcessingBook
-- add needed exports
where
  
import Data.Char

{-  BEGIN function definitions from section 7.6 of
    S. Thompson. _Haskell: The Craft of Functional Progamming_
    Third Edition, Addison Wesley, 2011.
-}

-- Type synonyms for words and lines. (Prime added to textbook version
-- to avoid name clash.)
type Word' = String
type Line' = [Word']

-- The "whitespace" characters.
whitespace :: String
whitespace = ['\n','\t',' ']

-- Get a word from the front of a string.
getWord :: String -> String
getWord []                = [] 
getWord (x:xs) 
    | elem x whitespace   = []
    | otherwise           = x : getWord xs
    
-- Drop the first word of a string.
dropWord :: String -> String
dropWord []               = []
dropWord (x:xs) 
    | elem x whitespace   = (x:xs)
    | otherwise           = dropWord xs

-- Remove the whitespace character(s) from the front of a string.
dropSpace :: String -> String
dropSpace []              = []
dropSpace (x:xs) 
    | elem x whitespace   = dropSpace xs
    | otherwise           = (x:xs)

-- Split a string into words.
splitWords :: String -> [Word']
splitWords st = split (dropSpace st)

split :: String -> [Word']
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

-- Split into lines of length at most lineLen.
lineLen :: Int
-- lineLen = 80
lineLen = 10   -- for testing purposes

-- Get a line from a list (name changed from Thompson book),
getLine2 :: Int -> [Word'] -> Line'
getLine2 len []           = []
getLine2 len (w:ws)
    | length w <= len     = w : restOfLine  
    | otherwise           = []
    where
        newlen      = len - (length w + 1)
        restOfLine  = getLine2 newlen ws

-- Get a line from a list, with error repaired.
-- If a word is longer than the total line length, textbook solution
-- for splitLines did not terminate.  The repair is to make getLine
-- keep the entire word as a line by itself.

getLine3 :: Int -> [Word'] -> Line'
getLine3 len []      = []
getLine3 len (w:ws)
    | lenword <= len = w : getLine3 newlen ws
    | len == lineLen = [w]  -- word too long, line by itself
    | otherwise      = []
    where
        lenword = length w
        newlen  = len - (lenword + 1)

-- Drop the first line from a list of words.
dropLine :: Int -> [Word'] -> Line'
dropLine len []     = []
dropLine len (w:ws) = drop (length (getLine3 len (w:ws))) (w:ws)

--dropLine :: Int -> [Word'] -> Line'
--dropLine len []      = []
--dropLine len (w:ws)
--    | lenword <= len = dropLine newlen ws
--    | len == lineLen = [w]
--    | otherwise      = (w:ws)
--    where
--        lenword = length w
--        newlen  = len - (lenword + 1)

-- Split into lines. (Changed to use getLine3.)
splitLines :: [Word'] -> [Line']
splitLines [] = []
splitLines ws = getLine3 lineLen ws
                    : splitLines (dropLine lineLen ws)

-- Fill a text string into lines.
fill :: String -> [Line']
fill xs = splitLines (splitWords xs)

-- END function definitions from Thompson textbook

-- CSCI 450
-- Homework 2
-- Tam Nguyen

import TextProcessingBook

-- 7.27
-- Define the function dropLine specified in the text
-- function written in "TextProcessingBook.hs", line 92

-- 7.28
-- Give a definition of the function
-- joinLine :: Line -> String
-- which turns a line into printable form. For example:
-- joinLine ["dog", "cat"] = "dog cat"
joinLine :: Line' -> String
joinLine []         = ""
joinLine (w:ws) 
    | length (w:ws) == 1 = w
    | otherwise          = w ++ " " ++ joinLine ws

-- 7.29
-- Using the fuction joinLine, or otherwise, define the function
-- joinLines :: [Line] -> String

-- 7.30
-- In this case study we have defined separate 'take' and 'drop' functions for words and lines.
-- Redesign the program so that it uses 'split' functions - like the prelude function splitAt - instead

-- 7.31
-- [Harder] Modify the function joinLine so that it justifies the line to length lineLen
-- by adding the appropriate number of spaces between the words.

-- 7.32
-- Design a function:
-- wc :: String -> (Int, Int, Int)
-- which when given a text string returns the number of characters, words, and lines in the string.
-- The end of a line in the string is signalled by the newline character '\n'.
-- Design a similar function:
-- wcFormat :: String -> (Int, Int, Int)
-- which returns the same statistics for the text after it has been filled.

-- 7.33
-- Define a function:
-- isPalin :: String -> Bool
-- which tests whether a string is a palindrome - that is whether it is the same read both backwards and forwards.
-- An example is the string: Madam I'm Adam
-- Note the punctuation and white space are ignored in the test, and that no distinction is made between capital and small letters.
-- You might first like to develop a test which simply tests the string is exactly the same backwards and forwards,
-- and only afterwards take account of punctation and capital letters.

-- 7.34
-- [Harder] Design a function:
-- subst :: String -> String -> String -> String
-- so that
-- subst oldSub newSub st
-- is the result of replacing the first occurence in st of the substring oldSub by the substring newSub.
-- For instance:
-- subst "much " "tall " "How much is that?" = "How tall is that?"
-- If the substring oldSub does not occur in st, the result should be st.
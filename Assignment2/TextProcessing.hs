-- CSCI 450
-- Homework 2
-- Tam Nguyen

module TextProcessing
where

import TextProcessingBook
import Data.Char
import Data.List
import Data.Maybe

-- 7.27: function written on line 92 in "TextProcessingBook.hs"
-- Define the function dropLine specified in the text
--dropLine :: Int -> [Word'] -> Line'
--dropLine len []      = []
--dropLine len (w:ws)
--    | lenword <= len = dropLine newlen ws
--    | len == lineLen = [w]
--    | otherwise      = (w:ws)
--    where
--        lenword = length w
--        newlen  = len - (lenword + 1)

-- 7.28
-- Give a definition of the function
-- joinLine :: Line -> String
-- which turns a line into printable form. For example:
-- joinLine ["dog", "cat"] = "dog cat"
joinLine :: Line' -> String
joinLine []              = ""
joinLine (w:ws) 
    | length (w:ws) == 1 = w
    | otherwise          = w ++ " " ++ joinLine ws

-- 7.29
-- Using the fuction joinLine, or otherwise, define the function
joinLines :: [Line'] -> String
joinLines []             = ""
joinLines (l:ls) 
    | length (l:ls) == 1 = joinLine l
    | otherwise          = joinLine l ++ "\n" ++ joinLines ls

-- 7.30
-- In this case study we have defined separate 'take' and 'drop' functions for words and lines.
-- Redesign the program so that it uses 'split' functions - like the prelude function splitAt - instead

-- 7.31
-- [Harder] Modify the function joinLine so that it justifies the line to length lineLen
-- by adding the appropriate number of spaces between the words.
-- The method is joinJustifiedLine. It takes in 2 arguments: the length of the line and the Line' to be joined.

getLineLength :: Line' -> Int
getLineLength [] = 0
getLineLength (w:ws)
    | length (w:ws) == 1 = length w
    | otherwise          = length w + 1 + getLineLength ws

constructJustifiedLine :: Int -> Int -> Line' -> String
constructJustifiedLine spaceLength numberOfLongerSpaces (w:ws)
    | length (w:ws) == 1        = w
    | numberOfLongerSpaces == 0 = w ++ replicate spaceLength ' ' ++ constructJustifiedLine spaceLength numberOfLongerSpaces ws
    | otherwise                 = w ++ replicate (spaceLength+1) ' ' ++ constructJustifiedLine spaceLength (numberOfLongerSpaces-1) ws

joinJustifiedLine :: Int -> Line' -> String
joinJustifiedLine lineLength [] = ""
joinJustifiedLine lineLength (w:ws)
    | differenceLength <= 0 = joinLine (w:ws)
    | otherwise             = constructJustifiedLine spaceLength numberOfLongerSpaces (w:ws)
    where
        lineLengthFromWords = getLineLength (w:ws)
        differenceLength = lineLength - lineLengthFromWords
        numberOfSpaces = length (w:ws) - 1
        spaceLength = (div differenceLength numberOfSpaces) + 1
        numberOfLongerSpaces = mod differenceLength numberOfSpaces

-- 7.32
-- Design a function:
-- wc :: String -> (Int, Int, Int)
-- which when given a text string returns the number of characters, words, and lines in the string.
-- The end of a line in the string is signalled by the newline character '\n'.
-- Design a similar function:
-- wcFormat :: String -> (Int, Int, Int)
-- which returns the same statistics for the text after it has been filled.
countLinesByCharacter :: String -> Int
countLinesByCharacter [] = 0
countLinesByCharacter (x:xs)
    | elem x ['\n']      = 1 + countLinesByCharacter xs
    | length (x:xs) == 1 = 1
    | otherwise          = countLinesByCharacter xs

wc :: String -> (Int, Int, Int)
wc [] = (0,0,0)
wc (x:xs) = (character, word, line)
    where
        character = length (x:xs)
        word = length (splitWords (x:xs))
        line = countLinesByCharacter (x:xs)

wcFormat :: String -> (Int, Int, Int)
wcFormat [] = (0,0,0)
wcFormat (x:xs) = (character, word, line)
    where
        formatString = joinLines (fill (x:xs))
        character = length formatString
        word = length (splitWords formatString)
        line = length (fill (x:xs))

-- 7.33
-- Define a function:
-- isPalin :: String -> Bool
-- which tests whether a string is a palindrome - that is whether it is the same read both backwards and forwards.
-- An example is the string: Madam I'm Adam
-- Note the punctuation and white space are ignored in the test, and that no distinction is made between capital and small letters.
-- You might first like to develop a test which simply tests the string is exactly the same backwards and forwards,
-- and only afterwards take account of punctation and capital letters.
--convertLower :: String -> String
--convertLower (x:xs) = toLower x : convertLower xs
specialCharacter = whitespace ++ ",.?!-:;\"\'{}[]><~`!@#$%^&*()_+=|"

removeAllSpecial :: String -> String
removeAllSpecial [] = []
removeAllSpecial (x:xs)
    | elem x specialCharacter = removeAllSpecial xs
    | otherwise         = x : removeAllSpecial xs

lowerCase :: String -> String
lowerCase = Prelude.map toLower

isPalin :: String -> Bool
isPalin w = lowerCase (removeAllSpecial w) == lowerCase (reverse (removeAllSpecial w))

-- 7.34
-- [Harder] Design a function:
-- subst :: String -> String -> String -> String
-- so that
-- subst oldSub newSub st
-- is the result of replacing the first occurence in st of the substring oldSub by the substring newSub.
-- For instance:
-- subst "much " "tall " "How much is that?" = "How tall is that?"
-- If the substring oldSub does not occur in st, the result should be st.
subst :: String -> String -> String -> String
subst oldSub newSub st
    | (isInfixOf oldSub st == False)= st
    | otherwise = joinLine(take (subIndex) stringList ++ subList ++ drop (subIndex+1) stringList)
        where
            subIndex = getIndex (getWord oldSub) st
            stringList = splitWords st
            subList = splitWords newSub

getIndex :: String -> String -> Int
getIndex oldSub st = fromMaybe 0 (elemIndex oldSub (splitWords st))

-- The subst function above only works when oldSub is a word without any space in between.
-- subst function won't work correctly when oldSub has space within. EX: "much is" as oldSub won't work.
-- The length and spaces in newSub won't affect the function.
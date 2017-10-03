-- CSCI 450
-- Homework 2
-- Tam Nguyen
module TextProcessingTest
where

import TextProcessing
import TextProcessingBook

main :: IO()
main =
    do
        putStrLn "7.27"
        putStrLn ("dropLine 10 [] = " ++ show(dropLine 10 []))
        putStrLn ("dropLine 10 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(dropLine 10 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("dropLine 12 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(dropLine 12 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("dropLine 13 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(dropLine 13 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("dropLine 20 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(dropLine 20 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ""

        putStrLn "7.28"
        putStrLn ("joinLine [] = " ++ show(joinLine []))
        putStrLn ("joinLine [\"Hello!\"] = " ++ show(joinLine ["Hello!"]))
        putStrLn ("joinLine [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"]         = " ++ show(joinLine ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("joinLine [\"Hello \", \"my \", \"name \", \"is \", \"Tam \"]    = " ++ show(joinLine ["Hello ", "my ", "name ", "is ", "Tam "]))
        putStrLn ""

        putStrLn "7.29"
        putStrLn ("joinLines [] = " ++ show(joinLines []))
        putStrLn ("joinLines [[], [], []] = " ++ show(joinLines [[], [], []]))
        putStrLn ("joinLines [[\"Hello\", \"my\", \"name\", \"is\", \"Tam\"]] = " ++ show(joinLines [["Hello", "my", "name", "is", "Tam"]]))
        putStrLn ("joinLines [[\"Hello\", \"my\", \"name\", \"is\", \"Tam.\"], [\"I\", \"am\", \"Vietnamese.\"]] = " ++ show(joinLines [["Hello", "my", "name", "is", "Tam"], ["I", "am", "Vietnamese."]]))
        putStrLn ("joinLines [[\"Hello\", \"my\", \"name\", \"is\", \"Tam.\"], [\"I\", \"am\", \"Vietnamese,\"], [\"and\", \"I\", \"love\", \"programming.\"]] = " ++ show(joinLines [["Hello", "my", "name", "is", "Tam"], ["I", "am", "Vietnamese,"], ["and", "I", "love", "programming."]]))
        putStrLn ""

        putStrLn "7.30"
        putStrLn "OMITTED."
        putStrLn ""

        putStrLn "7.31"
        putStrLn ("joinJustifiedLine 10 [] = " ++ show(joinJustifiedLine 10 []))
        putStrLn ("joinJustifiedLine 10 [\"Hello!\"] = " ++ show(joinJustifiedLine 10 ["Hello!"]))
        putStrLn ("joinJustifiedLine 10 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(joinJustifiedLine 10 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("joinJustifiedLine 20 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(joinJustifiedLine 20 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("joinJustifiedLine 22 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(joinJustifiedLine 22 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("joinJustifiedLine 30 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(joinJustifiedLine 30 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ("joinJustifiedLine 40 [\"Hello\", \"my\", \"name\", \"is\", \"Tam\"] = " ++ show(joinJustifiedLine 40 ["Hello", "my", "name", "is", "Tam"]))
        putStrLn ""

        putStrLn "7.32"
        putStrLn ("wc [] = " ++ show(wc []))
        putStrLn ("wc \"Hello my name is Tam\"                              = " ++ show(wc "Hello my name is Tam"))
        putStrLn ("wc \"Hello    my  name  is  Tam\"                        = " ++ show(wc "Hello    my  name  is  Tam"))
        putStrLn ("wc \"Hello my name is Tam.\\nI am Vietnamese.\"           = " ++ show(wc "Hello my name is Tam.\nI am Vietnamese."))
        putStrLn ("wc \"Hello    my  name  is  Tam.\\nI am Vietnamese.\"     = " ++ show(wc "Hello    my  name  is  Tam.\nI am Vietnamese."))
        putStrLn ("wcFormat [] = " ++ show(wcFormat []))
        putStrLn ("wcFormat \"Hello my name is Tam\"                        = " ++ show(wcFormat "Hello my name is Tam"))
        putStrLn ("wcFormat \"Hello    my  name  is  Tam\"                  = " ++ show(wcFormat "Hello    my  name  is  Tam"))
        putStrLn ("wcFormat \"Hello my name is Tam. I am Vietnamese.\"      = " ++ show(wcFormat "Hello my name is Tam. I am Vietnamese."))
        putStrLn ("wcFormat \"Hello    my  name  is  Tam. I am Vietnamese.\"= " ++ show(wcFormat "Hello    my  name  is  Tam. I am Vietnamese."))
        putStrLn ""

        putStrLn "7.33"
        putStrLn ("isPalin \"\"                          = " ++ show(isPalin ""))
        putStrLn ("isPalin \"Madam I'm Adam\"            = " ++ show(isPalin "Madam I'm Adam"))
        putStrLn ("isPalin \"Hello!\"                    = " ++ show(isPalin "Hello!"))
        putStrLn ("isPalin \"Mr. Owl Ate My Metal Worm\" = " ++ show(isPalin "Mr. Owl Ate My Metal Worm"))
        putStrLn ("isPalin \"Dammit, I'm Mad!\"          = " ++ show(isPalin "Dammit, I'm Mad!"))
        putStrLn ""

        putStrLn "7.34"
        putStrLn ("subst \"\" \"\" \"\" = " ++ show (subst "" "" ""))
        putStrLn ("subst \"much \" \"tall \" \"How much is that?\" = " ++ show (subst "much " "tall " "How much is that?"))
        putStrLn ("subst \"programming\" \"playing basketball\" \"I love programming\" = " ++ show (subst "programming" "playing basketball" "I love programming"))
        putStrLn ("subst \"Toddy\" \"Toddy Potty\" \"Hotty Toddy\" = " ++ show (subst "Toddy" "Toddy Potty" "Hotty Toddy"))
        putStrLn ""
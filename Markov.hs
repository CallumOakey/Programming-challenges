-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Main.hs"]



wordCouple :: (String, String)
linkedWords :: ((String, String), [String])


getText = do
    putStrLn "Hello, please give me text for learning how to speak!"
    text <- getLine
    putStrLn "Thx\n\n"

wordsList = words text

markov :: [String] -> [((String, String), [String])] -> [((String, String), [String])]
markov trainingText result
    |length trainingText >= 3 && length result > 0 && lookup (head result, head.tail result) /= Nothing = do

        markov (tail trainingText) result

    |length trainingText >= 3


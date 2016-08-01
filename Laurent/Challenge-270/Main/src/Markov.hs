-----------------------------------------------------------------------------
--
-- Module      :  Markov
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Laurent Ferier
-- Stability   :  Under development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Markov (
    buildMarkov,
    toString
) where

import System.Random

-- Builds a markov chains according to a given input
buildMarkov :: [String] ->  [((String, String), [String])] ->  [((String, String), [String])]
buildMarkov (x1:x2:x3:xs) chain = buildMarkov (x2:x3:xs) (addToChain chain (x1, x2) x3)
buildMarkov _             chain = chain

-- Adds a element in the markov chain
addToChain :: [((String, String), [String])] -> (String, String) -> String -> [((String, String), [String])]
addToChain (((x1, x2), choices):xs) (x3, x4) item
   | x1 == x3 && x2 == x4    = ((x1, x2), choices ++ [item]):xs
   | otherwise               = ((x1, x2), choices):(addToChain xs (x3, x4)) item
addToChain _ (x1, x2) item   = [((x1, x2), [item])]

-- Converts a Markov chain into its string representation
toString :: [((String, String), [String])] -> String
toString (((x1,x2),choices):xs) = "(" ++ x1 ++ ", " ++ x2 ++ ") -> " ++ (listToString choices) ++ "\n" ++ (toString xs)
toString _                      = ""

-- Converts a list into its corresponding string
listToString :: [String] -> String
listToString (x:xs) = x ++ listToString xs
listToString _      = ""

chooseInList :: StdGen -> [a] -> a
chooseInList gen list = ""

chooseOutput :: StdGen -> [((String, String), [String])] -> String -> String
chooseOutput   _                     []  _  _ = ""
chooseOutput gen (((s1, s2), items):xs) s1 s2 = chooseInList gen items
chooseOutput gen                 (x:xs) s1 s2 = chooseOutput gen xs s1 s2

-- Generates the text (of a given length) according to a start element
generate :: StdGen -> [((String, String), [String])] -> ((String, String), [String]) -> Int -> String
generate   _      _       _     0 = ""
generate gen markov element count =
    chooseOuput gen markov

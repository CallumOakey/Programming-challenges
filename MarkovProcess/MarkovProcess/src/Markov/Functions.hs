-----------------------------------------------------------------------------
--
-- Module      :  Markov.Functions
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

module Markov.Functions (
    markovResList,
    markovWordCoupleList,
    markov
) where

-- Takes a training text and a word couple and returns the results string
-- associated to the word couple by going through the training text and adding
-- words to the result when they follow the word couple.
markovResList :: [String] -> (String, String) -> [String]
markovResList (x:y:z:xs) aWordCouple
    | aWordCouple == (x, y) = z : markovResList (y:z:xs) aWordCouple
    | otherwise = markovResList (y:z:xs) aWordCouple
markovResList _ aWordCouple = []

-- Takes the full words couple list from a training text and an accumulator,
-- and returns the same list after having removed all duplicates.
markovWordCoupleList :: [(String, String)] -> [(String, String)] -> [(String, String)]
markovWordCoupleList (x:xs) res
    | not (elem x res) = x : markovWordCoupleList xs (x:res)
    | otherwise = markovWordCoupleList xs res
markovWordCoupleList _ res = []

-- Uses markovResList to associate to each word couple its list of possible results.
markov :: [String] -> [(String, String)] -> [((String, String) , [String])]
markov (x:xs) (aWordCouple:rest) = (aWordCouple, markovResList (x:xs) aWordCouple) : markov xs rest
markov _ [] = []

-- Simple count function.

count :: String -> [String] -> Int
count a = length . filter (a==)

-- Create a function to assign the probability values to all of the possibilities of each word couple, based 
-- on the number of occurences of each word. This could be done by improving markovResList rather than creating
-- a whole new funtion that will go through the whole text again.

countResults :: [String] -> [String] -> [(String, Int)]
countResults (x:xs) acc
    | not (elem x acc) = (x, count x (x:xs)) : countResults xs (x:acc)
    | otherwise = countResults xs acc
countResults _ acc = []

-- Create a choose 3rd word function that takes a word couple and picks a random 3rd word based on the 
-- probabilities established in the markov set.



-- Create a choose random word coule funtion, as it will be used in both the initialise and
-- generate functions.



-- Create initialise function that takes the Markov set, chooses a word couple at random, and then 
-- chooses a 3rd word out of the possibilities.




-- Create a generate function that takes the last 2 words and gives out a 3rd from the list of possibilities.
-- If the last word couple is not one of the options, then give out a randome word couple.


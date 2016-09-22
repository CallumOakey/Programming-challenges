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
    markov,
    count,
    countResults,
    countMarkovSetResults,
    randWordCouple
) where

import System.Random

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

-- Assigns the probability values to each different word of a result list, based 
-- on the number of occurences of each word.

countResults :: [String] -> [String] -> [(String, Int)]
countResults (x:xs) acc
    | not (elem x acc) = (x, count x (x:xs)) : countResults xs (x:acc)
    | otherwise = countResults xs acc
countResults _ acc = []

-- Replaces the results list in the markov set with a list of anly all the different words, but associated with the number of 
-- times it occurs.

countMarkovSetResults :: [((String, String) , [String])] -> [((String, String) , [(String, Int)])]
countMarkovSetResults (x:xs) = (fst x, countResults (snd x) []) : countMarkovSetResults xs
countMarkovSetResults _ = []

-- Random number generator within range

rand :: Int -> StdGen -> (Int, StdGen)
rand a gen = randomR (0,a) gen

-- Create a choose random word coule funtion, as it will be used in both the initialise and
-- generate functions.

randWordCouple :: [((String, String) , [(String, Int)])] -> StdGen -> ((String, String), StdGen)
randWordCouple (x:xs) gen = (fst ((x:xs) !! (fst (rand ((length (x:xs)) - 1) gen))), snd (rand ((length (x:xs)) - 1) gen) )
randWordCouple _ gen = (("",""), snd (rand 1 gen) ) -- Should be an error. Must look into custom errors.

-- Create a choose 3rd word function that takes a word couple and picks a random 3rd word based on the 
-- probabilities established in the markov set.



-- Create initialise function that takes the Markov set, chooses a word couple at random, and then 
-- chooses a 3rd word out of the possibilities.




-- Create a generate function that takes the last 2 words and gives out a 3rd from the list of possibilities.
-- If the last word couple is not one of the options, then give out a randome word couple.


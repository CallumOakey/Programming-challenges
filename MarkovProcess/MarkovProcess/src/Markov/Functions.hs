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

-- Couples the word couples with their associated results string
markov :: [String] -> [(String, String)] -> [((String, String) , [String])]
markov (x:xs) (aWordCouple:rest) = (aWordCouple, markovResList (x:xs) aWordCouple) : markov xs rest
markov _ [] = []

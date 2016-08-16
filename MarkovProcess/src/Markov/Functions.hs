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

) where

-- Returns the results string associated to a word couple
markov :: [String] -> [(String, String)] -> [String]
markovResList (x:y:xs) a
    | a == (x:y) = head xs : markovResList (y:xs) a
    | otherwise = markovResList (y:xs) a
markovResList _ a = []

-- Returns the list of word couples from a text
markovWordCoupleList :: [String] -> [(String, String)] -> [(String, String)]
markovWordCoupleList (x:y:xs) res
    | lookup (x:y) res == Nothing = (x:y) : markovWordCoupleList (y:xs) ((x:y) : res)
    | otherwise = markovWordCoupleList (y:xs) res
markovWordCoupleList _ res = []

-- Couples the word couples with their associated results string
markov :: [String] -> [(String, String)] -> [((String, String) , [String])]
markov (x:xs) (a:as) = (a, markovResList (x:xs) a) : markov xs as
markov _ [] = []

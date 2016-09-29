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
    randWordCouple,
    markovResult
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

-- Choose random word coule funtion, as it will be used in both the initialise and
-- generate functions.

randWordCouple :: [((String, String) , [(String, Int)])] -> StdGen -> ((String, String), StdGen)
randWordCouple markovSet gen = 
    let randNumber = rand ((length markovSet) - 1) gen
    in (fst (markovSet !! (fst randNumber)), snd randNumber )
    
-- Extract value from a maybe.

extract :: Maybe [(String, Int)] -> [(String, Int)]
extract (Just a) = a
extract Nothing = [("", 0)]

-- Returns the word chosen by the random number in the results list of a word couple.

resWord :: Int -> [(String, Int)] -> String
resWord number ((word,count):xs)
    | number - count > 0 = resWord (number - count) xs
    | number - count <= 0 = word

-- Choose 3rd word function that takes a word couple and picks a random 3rd word based on the 
-- probabilities established in the markov set.

thirdWord :: [((String, String) , [(String, Int)])] -> (String,String) -> StdGen -> (String, StdGen)
thirdWord markovSet couple gen = 
    let resList = extract(lookup couple markovSet) -- extract can be used here since the word couple's existence is verified in generateNext.
        probabilitiesLength = foldr (+) 0 (map (snd) resList)
        randNumber = rand probabilitiesLength gen
        res = resWord (fst randNumber) resList
    in (res , snd randNumber)

-- Generate function that takes the last 2 words and gives out a 3rd from the list of possibilities.
-- If the last word couple is not one of the options, then give out a randome word couple.

generateNext :: [((String, String) , [(String, Int)])] -> [String] -> Int -> StdGen -> ([String], StdGen)
generateNext markovSet (x:y:xs) len gen
    | len <= 0 = ((x:y:xs), gen)
--    | lookup (last (init (x:y:xs)), last (x:y:xs)) markovSet == Nothing = 
--        let resCouple = randWordCouple markovSet gen
--        in generateNext markovSet ((x:y:xs) ++ [fst(fst resCouple), snd(fst resCouple)]) (len - 2) (snd resCouple)
    | otherwise = 
        let res = thirdWord markovSet (last (init (x:y:xs)), last (x:y:xs)) gen
        in generateNext markovSet ((x:y:xs) ++ [fst res]) (len - 1) (snd res)
generateNext markovSet [] len gen = 
        let resCouple = randWordCouple markovSet gen
        in generateNext markovSet ([fst(fst resCouple), snd(fst resCouple)]) (len - 2) (snd resCouple)

-- markovResult function that takes the Markov set and recurcively generates text.

markovResult :: [((String, String) , [(String, Int)])] -> Int -> StdGen -> [String]
markovResult markovSet len gen = fst (generateNext markovSet [] len gen)









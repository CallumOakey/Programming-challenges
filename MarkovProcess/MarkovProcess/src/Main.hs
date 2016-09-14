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

import System.IO
import Markov.Functions
import Test.HUnit

main :: IO ()
main = do
    runTestTT tests
    handle <- openFile "ab.txt" ReadMode
    trainingText <- hGetContents handle 
    let wordsList = words trainingText
    let wordsZip = zip (init wordsList) (tail wordsList)
    let wordCouplesList = markovWordCoupleList wordsZip []
    let markovSet = markov wordsList wordCouplesList
    print markovSet
    return ()
    
--Tests setup

text = "a b a a b b a b"
textWords = words text
textZip = zip (init textWords) (tail textWords)

test1 =  markovWordCoupleList textZip [] ~=? [("a","b"), ("b","a"), ("a","a"), ("b","b")]
test2 =  markov textWords (markovWordCoupleList textZip []) ~=? [(("a","b"),["a","b"]),(("b","a"),["a","b"]),(("a","a"),["b"]),(("b","b"),["a"])]

tests = TestList [ TestLabel "Test1" test1, TestLabel "Test2" test2  ]

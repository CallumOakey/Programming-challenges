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
import System.Random

main :: IO ()
main = do
    runTestTT tests
    handle <- openFile "source.txt" ReadMode
    trainingText <- hGetContents handle 
    let wordsList = words trainingText
        wordsZip = zip (init wordsList) (tail wordsList)
        wordCouplesList = markovWordCoupleList wordsZip []
        markovSet = countMarkovSetResults (markov wordsList wordCouplesList)
    print markovSet
    return ()
    
--Tests setup

text = "a b a a b b a b"
textWords = words text
textZip = zip (init textWords) (tail textWords)

test1 =  markovWordCoupleList textZip [] ~=? [("a","b"), ("b","a"), ("a","a"), ("b","b")]
test2 =  markov textWords (markovWordCoupleList textZip []) ~=? [(("a","b"),["a","b"]),(("b","a"),["a","b"]),(("a","a"),["b"]),(("b","b"),["a"])]
test3 =  countResults ["a","b","b","a","a","b","a","a","b"] [] ~=? [("a",5),("b",4)]
test4 =  countMarkovSetResults [(("a","b"),["a","b"]),(("b","a"),["a","b"]),(("a","a"),["b"]),(("b","b"),["a"])] ~=? [(("a","b"),[("a",1),("b",1)]),(("b","a"),[("a",1),("b",1)]),(("a","a"),[("b",1)]),(("b","b"),[("a",1)])]
test5 =  randWordCouple [(("a","b"),[("a",1),("b",1)]),(("b","a"),[("a",1),("b",1)]),(("a","a"),[("b",1)]),(("b","b"),[("a",1)])] (mkStdGen 50222222000)

tests = TestList [ TestLabel "Test1" test1, TestLabel "Test2" test2, TestLabel "Test3" test3, TestLabel "Test4" test4 ]

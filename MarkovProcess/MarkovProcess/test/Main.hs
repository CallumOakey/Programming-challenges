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

import Test.HUnit
import Markov.Functions

text = "a b a a b b a b"
wordsList = words text
textZip = zip (init wordsList) (tail wordsList)

test1 =  markovWordCoupleList textZip [] ~=? [("a","b"), ("b","a"), ("a","a"), ("b","b")]
test2 =  markov wordsList wordCouplesList ~=? [(("a","b"),["a","b"]),(("b","a"),["a","b"]),(("a","a"),["b"]),(("b","b"),["a"])]

tests = TestList [ TestLabel "Test1" test1, TestLabel "Test2" test2  ]

-- Entry point of the tests
main :: IO ()
main = do
    runTestTT tests
    return ()



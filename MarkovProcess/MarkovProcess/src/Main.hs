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

main :: IO ()
main = do
    text <- return "a b a a b b a b"
    let wordsList = words text
    let textZip = zip (init wordsList) (tail wordsList)
    let wordCouplesList = markovWordCoupleList textZip []
    let markovSet = markov wordsList wordCouplesList
    print markovSet

testText = ["a", "b", "a", "a", "b", "b", "a", "b"]
zipTestText = zip (init testText) (tail testText)

testMarkovResList = markovResList testText ("a", "b")
testMarkovWordCoupleList = markovWordCoupleList zipTestText []
testMarkov = markov testText testMarkovWordCoupleList

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
    text <- getContents
    let wordsList = words text
    print wordsList
    let textZip = zip (init wordsList) (tail wordsList)
    print textZip
    let wordCouplesList = markovWordCoupleList textZip []
    print wordCouplesList
    let markovSet = markov wordsList wordCouplesList
    print markovSet
    return ()

testText = ["a", "b", "a", "a", "b", "b", "a", "b"]
zipTestText = zip (init testText) (tail testText)

testMarkovResList = markovResList testText ("a", "b")
testMarkovWordCoupleList = markovWordCoupleList zipTestText []
testMarkov = markov testText testMarkovWordCoupleList

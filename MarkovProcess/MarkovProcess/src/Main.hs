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
    wordsList <- words text
    textZip <- zip (init wordsList) (tail wordsList)
    wordCouplesList <- markovWordCoupleList textZip []
    markovSet <- markov wordsList wordCouplesList
    return markovSet
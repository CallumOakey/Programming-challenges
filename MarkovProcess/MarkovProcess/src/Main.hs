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

main :: IO [()]
main = do
    text <- getContents
    let wordsList = words text
    let textZip = zip (init wordsList) (tail wordsList)
    let wordCouplesList = markovWordCoupleList textZip []
    let markovSet = markov wordsList wordCouplesList
    sequence (map print markovSet)

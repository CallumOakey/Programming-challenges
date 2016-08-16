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

import Markov.Functions

main :: IO ()
main = do
    text <- getContents
    wordsList = words text
    res = markov wordList (markovWordCoupleList wordsList [])
    print res

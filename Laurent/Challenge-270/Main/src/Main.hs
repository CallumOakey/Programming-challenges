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
import System.Random
import Markov

-- Picks a random element from a list
pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)

-- Entry point of the executable
main :: IO ()
main = do
   putStrLn "Markov process utility"
   withFile "source.txt" ReadMode (\handle -> do
        putStrLn "File read"
        contents <- hGetContents handle
        putStrLn ("The file contains " ++ (show (length (words contents))) ++ " words")
        putStrLn ("The markov chain contains " ++ (show (length (Markov.buildMarkov (words contents) []))) ++ " entries")
        -- start <- pick (Markov.buildMarkov (words contents) [])
        )

-----------------------------------------------------------------------------
--
-- Module      :  Markov
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Laurent Ferier
-- Stability   :  Under development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (

) where

import Test.HUnit
import Markov

test1 = Markov.listToString ["a", "b"] ~=? "ab"
test2 = Markov.listToString ["a", "b"] ~=? "az"

tests = TestList [ TestLabel "Test1" test1, TestLabel "Test2" test2  ]

-- Entry point of the tests
main :: IO ()
main = do
    runTestTT tests
    return ()

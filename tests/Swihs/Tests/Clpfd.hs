{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Swihs.Tests.Clpfd where

import Swihs
import Swihs.Lib.Clpfd
import Test.Tasty
import Test.Tasty.HUnit

clpfdTests :: TestTree
clpfdTests =
    testGroup "CLP(FD)"
    [ testCase "Arithmetics" $ do
        sol <- queryList $ 1 + 1 #= "X"
        sol @=? [[("X", Ground 2)]]
    , testCase "Reverse arithmetics" $ do
        sol <- queryList $ 1 + "X" #= 5
        sol @=? [[("X", Ground 4)]]
    , testCase "Sum triples" $ do
        sol <- queryList $ "X" `in_` 1...10
                        .& "Y" `in_` 1...10
                        .& "Z" `in_` 1...10
                        .& "X" + "Y" #= "Z"
                        .& label ["X", "Y", "Z"]
        let good = [("X", Ground 4), ("Y", Ground 3), ("Z", Ground 7)]
            bad = [("X", Ground 1), ("Y", Ground 1), ("Z", Ground 3)]
        assertBool "4 + 3 #= 7 is a solution" (good `elem` sol)
        assertBool "1 + 1 #= 3 is not a solution" (not (elem bad sol))
    ]

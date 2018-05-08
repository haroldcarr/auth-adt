{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Protolude hiding (show, Hashable(..))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Auth
import Hash

main :: IO ()
main = defaultMain tests

data Tree a
  = Tip a
  | Bin (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

exampleTree :: Tree Int
exampleTree = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

testGenericProof :: Tree Int -> Int -> Bool
testGenericProof tree a = verify rootHash proof a
    where
        proof = prove tree a
        rootHash = toHash tree

tests :: TestTree
tests = testGroup "Authenticated Data Structures"
  [ testCase "verify valid element in proof should return true" $
        assertBool "1, 2, 3, 5, 8 should be in the proof" $ 
            not $ elem False 
                [ testGenericProof exampleTree 3
                , testGenericProof exampleTree 5
                , testGenericProof exampleTree 2
                , testGenericProof exampleTree 1
                , testGenericProof exampleTree 8
                ]
  , testCase "Verify invalid proof should return false" $
        assertBool "4, 7, 11, 18, 29 should not be in the proof" $
            not $ elem True
                [ testGenericProof exampleTree 4
                , testGenericProof exampleTree 7
                , testGenericProof exampleTree 11
                , testGenericProof exampleTree 18
                , testGenericProof exampleTree 29
                ]

  ]


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
import System.Random
import TreeTests

import Auth
import Hash

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Authenticated Data Structures"
  [ treeTests 
  ]



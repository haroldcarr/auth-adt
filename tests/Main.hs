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
import Crypto.Number.Generate (generateBetween)

import Auth
import Hash

main :: IO ()
main = defaultMain tests

data Tree a
  = Tip a
  | Bin (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    m <- genPos
    arbitrarySizedTree m

genPosUnique :: Tree Int -> Gen Int
genPosUnique t = do
    a <- abs <$> arbitrary `suchThat` (> 1)
    if findElem a t then genPosUnique t else pure a

genPos :: Gen Int
genPos = abs <$> arbitrary `suchThat` (> 1)

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  a <- arbitrary
  n <- choose (1, m - 1)
  case m of
    1 -> pure $ Tip a
    otherwise -> Bin <$> (arbitrarySizedTree n) <*> (arbitrarySizedTree (m - n))

exampleTree :: Tree Int
exampleTree = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

findElem :: Eq a => a -> Tree a -> Bool 
findElem a (Tip b) = a == b
findElem a (Bin l r) = findElem a l || findElem a r

replaceRandomElem :: Eq a => a -> Tree a -> IO (Tree a)
replaceRandomElem a (Tip b) = pure $ Tip a
replaceRandomElem a (Bin l r) = do
    b <- randomIO
    if b 
        then Bin <$> replaceRandomElem a l <*> pure r
        else Bin <$> pure l <*> replaceRandomElem a r

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
   , testProperty "Verify arbitrarily sized trees" $
        forAll arbitrary $ \t ->
        forAll (genPosUnique t) $ \a -> testTreeProperty a t
  ]

testTreeProperty :: Int -> Tree Int -> Property
testTreeProperty a tree = monadicIO $ do 
    tree' <- liftIO $ replaceRandomElem a tree
    pure $ testGenericProof tree' a

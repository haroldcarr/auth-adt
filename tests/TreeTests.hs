{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module TreeTests where

import Protolude hiding (show, Hashable(..))
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import System.Random

import Auth
import Hash

data Tree a
  = Tip a
  | Bin (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

exampleTreeInt :: Tree Int
exampleTreeInt = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    m <- genPos
    arbitrarySizedTree m

instance Arbitrary ByteString where
    arbitrary = toS <$> (arbitrary :: Gen [Char]) `suchThat` (/= "") 

testGenericProof :: (Hashable a, Eq a) => Tree a -> a -> Bool
testGenericProof tree a = verify rootHash proof a
    where
        proof = prove tree a
        rootHash = toHash tree

testTreeProperty :: (Hashable a, Eq a) => a -> Tree a -> Property
testTreeProperty a tree = monadicIO $ do
    tree' <- liftIO $ replaceRandomElem a tree
    pure $ testGenericProof tree' a

treeTests :: TestTree
treeTests = testGroup "Trees Tests"
  [ testCase "Verify valid element in proof should return true" $
        assertBool "1, 2, 3, 5, 8 should be in the proof" $ 
            not $ elem False 
                [ testGenericProof exampleTreeInt 3
                , testGenericProof exampleTreeInt 5
                , testGenericProof exampleTreeInt 2
                , testGenericProof exampleTreeInt 1
                , testGenericProof exampleTreeInt 8
                ]
  , testCase "Verify invalid proof should return false" $
        assertBool "4, 7, 11, 18, 29 should not be in the proof" $
            not $ elem True
                [ testGenericProof exampleTreeInt 4
                , testGenericProof exampleTreeInt 7
                , testGenericProof exampleTreeInt 11
                , testGenericProof exampleTreeInt 18
                , testGenericProof exampleTreeInt 29
                ]
   , testProperty "Verify arbitrarily sized trees of ints" $
        forAll arbitrary $ \t ->
        forAll (genPosUnique t) $ \a -> testTreeProperty a t

   ,  localOption (QuickCheckTests 10) $
        testProperty "Verify arbitrarily sized trees of bytestrings" $
            (testTreeProperty :: ByteString -> Tree ByteString -> Property)
  ]

genPosUnique :: Tree Int -> Gen Int
genPosUnique t = do
    a <- abs <$> arbitrary `suchThat` (> 1)
    if findElem a t then genPosUnique t else pure a

genBSUnique :: Tree ByteString -> Gen ByteString
genBSUnique t = do
    a <- arbitrary
    if findElem a t then genBSUnique t else pure a

genPos :: Gen Int
genPos = abs <$> arbitrary `suchThat` (> 1)

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  a <- arbitrary
  n <- choose (1, m - 1)
  case m of
    1 -> pure $ Tip a
    otherwise -> Bin <$> (arbitrarySizedTree n) <*> (arbitrarySizedTree (m - n))

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

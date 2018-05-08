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

data BinTree a
  = Tip a
  | Bin (BinTree a) (BinTree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

exampleBinTreeInt :: BinTree Int
exampleBinTreeInt = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = do
    m <- genPos
    arbitrarySizedBinTree m

instance Arbitrary ByteString where
    arbitrary = toS <$> (arbitrary :: Gen [Char]) `suchThat` (/= "") 

testGenericProof :: (Hashable a, Eq a) => BinTree a -> a -> Bool
testGenericProof tree a = verifyProof rootHash proof a
    where
        proof = prove tree a
        rootHash = toHash tree

testBinTreeProperty :: (Hashable a, Eq a) => a -> BinTree a -> Property
testBinTreeProperty a tree = monadicIO $ do
    tree' <- liftIO $ replaceRandomElem a tree
    pure $ testGenericProof tree' a

treeTests :: TestTree
treeTests = testGroup "Tree Tests"
  [ testCase "Verify valid element in proof should return true" $
        assertBool "1, 2, 3, 5, 8 should be in the proof" $ 
            not $ elem False 
                [ testGenericProof exampleBinTreeInt 3
                , testGenericProof exampleBinTreeInt 5
                , testGenericProof exampleBinTreeInt 2
                , testGenericProof exampleBinTreeInt 1
                , testGenericProof exampleBinTreeInt 8
                ]
  , testCase "Verify invalid proof should return false" $
        assertBool "4, 7, 11, 18, 29 should not be in the proof" $
            not $ elem True
                [ testGenericProof exampleBinTreeInt 4
                , testGenericProof exampleBinTreeInt 7
                , testGenericProof exampleBinTreeInt 11
                , testGenericProof exampleBinTreeInt 18
                , testGenericProof exampleBinTreeInt 29
                ]
   , testProperty "Verify arbitrarily sized trees of ints" $
        forAll arbitrary $ \t ->
        forAll (genPosUnique t) $ \a -> testBinTreeProperty a t

   ,  localOption (QuickCheckTests 10) $
        testProperty "Verify arbitrarily sized trees of bytestrings" $
            (testBinTreeProperty :: ByteString -> BinTree ByteString -> Property)
  ]

genPosUnique :: BinTree Int -> Gen Int
genPosUnique t = do
    a <- abs <$> arbitrary `suchThat` (> 1)
    if findElem a t then genPosUnique t else pure a

genBSUnique :: BinTree ByteString -> Gen ByteString
genBSUnique t = do
    a <- arbitrary
    if findElem a t then genBSUnique t else pure a

genPos :: Gen Int
genPos = abs <$> arbitrary `suchThat` (> 1)

arbitrarySizedBinTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitrarySizedBinTree m = do
  a <- arbitrary
  n <- choose (1, m - 1)
  case m of
    1 -> pure $ Tip a
    otherwise -> Bin <$> (arbitrarySizedBinTree n) <*> (arbitrarySizedBinTree (m - n))

findElem :: Eq a => a -> BinTree a -> Bool
findElem a (Tip b) = a == b
findElem a (Bin l r) = findElem a l || findElem a r

replaceRandomElem :: Eq a => a -> BinTree a -> IO (BinTree a)
replaceRandomElem a (Tip b) = pure $ Tip a
replaceRandomElem a (Bin l r) = do
    b <- randomIO
    if b
        then Bin <$> replaceRandomElem a l <*> pure r
        else Bin <$> pure l <*> replaceRandomElem a r

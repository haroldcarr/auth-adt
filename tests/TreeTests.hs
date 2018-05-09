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

----------------------------
-- Tests
----------------------------

testGenericProof :: (Authable f, Hashable (f a), Hashable a, Eq a) => f a -> a -> Bool
testGenericProof tree a = verifyProof rootHash proof a
    where
        proof = prove tree a
        rootHash = toHash tree

testBinTreeProperty :: (Hashable a, Eq a) => a -> BinTree a -> Property
testBinTreeProperty a tree = monadicIO $ do
    tree' <- liftIO $ replaceBinElem a tree
    pure $ testGenericProof tree' a


treeTests :: TestTree
treeTests = testGroup "Create and verify proof in tree structures"
  [ testGroup "Binary Trees"
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
  , testGroup "Ternary Trees"
    [ testCase "Verify valid element in proof should return true" $
        assertBool "1, 2, 3, 5, 8 should be in the proof" $
            not $ elem False
                [ testGenericProof exampleTernaryTreeInt 3
                , testGenericProof exampleTernaryTreeInt 5
                , testGenericProof exampleTernaryTreeInt 2
                , testGenericProof exampleTernaryTreeInt 1
                , testGenericProof exampleTernaryTreeInt 8
                ]
    , testCase "Verify invalid proof should return false" $
        assertBool "4, 7, 11, 18, 29 should not be in the proof" $
            not $ elem True
                [ testGenericProof exampleTernaryTreeInt 4
                , testGenericProof exampleTernaryTreeInt 7
                , testGenericProof exampleTernaryTreeInt 11
                , testGenericProof exampleTernaryTreeInt 18
                , testGenericProof exampleTernaryTreeInt 29
                ]
    ]
  ]

-----------------------------
-- Binary trees
-----------------------------

data BinTree a
  = Tip a
  | Bin (BinTree a) (BinTree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

exampleBinTreeInt :: BinTree Int
exampleBinTreeInt = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

-- Arbitrary

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = do
    m <- genPos
    arbitrarySizedBinTree m

instance Arbitrary ByteString where
    arbitrary = toS <$> (arbitrary :: Gen [Char]) `suchThat` (/= "")

-- | Generate a positive number that does not exist in the tree
genPosUnique :: BinTree Int -> Gen Int
genPosUnique t = do
    a <- abs <$> arbitrary `suchThat` (> 1)
    if findBinElem a t then genPosUnique t else pure a

-- | Generate a bytestring that does not exist in the tree
genBSUnique :: BinTree ByteString -> Gen ByteString
genBSUnique t = do
    a <- arbitrary
    if findBinElem a t then genBSUnique t else pure a

-- | Generate a positive number
genPos :: Gen Int
genPos = abs <$> arbitrary `suchThat` (> 1)

-- | Generate a randomly sized binary tree
arbitrarySizedBinTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitrarySizedBinTree m = do
  a <- arbitrary
  n <- choose (1, m - 1)
  case m of
    1 -> pure $ Tip a
    otherwise -> Bin <$> (arbitrarySizedBinTree n) <*> (arbitrarySizedBinTree (m - n))

findBinElem :: Eq a => a -> BinTree a -> Bool
findBinElem a (Tip b) = a == b
findBinElem a (Bin l r) = findBinElem a l || findBinElem a r

replaceBinElem :: Eq a => a -> BinTree a -> IO (BinTree a)
replaceBinElem a (Tip b) = pure $ Tip a
replaceBinElem a (Bin l r) = do
    b <- randomIO
    if b
        then Bin <$> replaceBinElem a l <*> pure r
        else Bin <$> pure l <*> replaceBinElem a r

-----------------------------
-- Ternary trees
-----------------------------

data TernaryTree a
  = Tip' a
  | Ternary (TernaryTree a) (TernaryTree a) (TernaryTree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

exampleTernaryTreeInt :: TernaryTree Int
exampleTernaryTreeInt =
  Ternary (Ternary (Tip' 1) (Tip' 3) (Tip' 8)) (Tip' 5) (Ternary (Tip' 9) (Ternary (Tip' 2) (Tip' 12) (Tip' (-5))) (Tip' (-4)))


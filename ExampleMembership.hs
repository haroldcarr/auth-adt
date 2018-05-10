{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module ExampleAuth where

import Protolude hiding (Hashable)

import Hash
import Membership

data BinTree a
  = Tip a
  | Bin (BinTree a) (BinTree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)

myBinTree :: BinTree Int
myBinTree = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

binTreeExample :: IO Bool
binTreeExample = do
  let member = 3
  -- Prove membership
  let proof = prove myBinTree member
  -- Verifier only keeps the hash of the root
  let rootHash = toHash myBinTree
  -- Verify proof
  pure $ verifyProof rootHash proof member

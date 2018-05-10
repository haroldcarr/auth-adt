{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module AuthenticatedTests where

import Protolude hiding (Hashable)

import Hash
import Authenticated
import Auth hiding (L, R)

data Tree a
  = Tip a
  | Bin (Auth (Tree a)) (Auth (Tree a))
  deriving (Eq, Functor,Generic, Generic1, Show, Hashable)

instance Shallow Tree


{-instance Shallow (Tree a) where-}
    {-shallow (Tip s) = Tip s-}
    {-shallow (Bin l r) = Bin (shallow l) (shallow r)-}

data Bit = L | R

fetch :: (Hashable a) => [Bit] -> Auth (Tree a) -> AuthM (Tree a) (Maybe a)
fetch idx authTree = do
  tree <- (unAuth authTree)
  case (idx, tree) of
    ([]      , Tip s  ) -> return $ Just s
    (L : idx', Bin l _) -> fetch idx' l
    (R : idx', Bin _ r) -> fetch idx' r
    _ -> return Nothing

bin :: Hashable a => Auth (Tree a) -> Auth (Tree a) -> Auth (Tree a)
bin l r = auth (Bin l r)

tip :: Hashable a => a -> Auth (Tree a)
tip v = auth $ Tip v

tree :: Auth (Tree Text)
tree = bin (bin (tip "b") (bin (tip "c") (bin (tip "d") (tip "c")))) (tip "a")

shallowTree :: Auth (Tree Text)
shallowTree = shallow tree

example :: IO ()
example = do
  let proof = snd $ runProver  $ fetch [L,R,L] tree
  print $ runVerifier (fetch [L,R,L] shallowTree) proof
  print $ runVerifier (fetch [R] shallowTree) proof


{-instance Arbitrary a => Arbitrary (BinTree a) where-}
  {-arbitrary = do-}


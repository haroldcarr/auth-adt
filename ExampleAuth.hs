{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module ExampleAuth where

import Protolude hiding (Hashable)

import Hash
import Authenticated


data Tree a
  = Tip a
  | Bin (Auth (Tree a)) (Auth (Tree a))
  deriving (Eq, Functor, Generic, Generic1, Show, Hashable)

bin :: Hashable a => Auth (Tree a) -> Auth (Tree a) -> Auth (Tree a)
bin l r = auth (Bin l r)

tip :: Hashable a => a -> Auth (Tree a)
tip v = auth $ Tip v

instance Shallow (Tree a) where
    shallow (Tip s) = Tip s
    shallow (Bin l r) = Bin (shallow l) (shallow r)

data Bit = L | R

fetch
  :: Hashable a
  => [Bit] -- ^ the path to find the element at
  -> Auth (Tree a)
  -> AuthM (Tree a) (Maybe a)
fetch idx authTree = do
  tree <- unAuth authTree
  case (idx, tree) of
    ([]      , Tip s  ) -> return $ Just s
    (L : idx', Bin l _) -> fetch idx' l
    (R : idx', Bin _ r) -> fetch idx' r
    _                   -> return Nothing

update
  :: Hashable a
  => [Bit] -- ^ the path of the element to update
  -> Auth (Tree a)
  -> a
  -> AuthM (Tree a) (Maybe (Auth (Tree a)))
update idx authTree elem = do
  tree <- unAuth authTree
  case (idx, tree) of
    ([]      , Tip s  ) -> return $ Just (tip elem)
    (L : idx', Bin l r) -> do
      l' <- update idx' l elem
      return $ fmap (`bin` r) l'
    (R : idx', Bin l r) -> do
      r' <- update idx' r elem
      return $ fmap (bin l) r'
    _ -> return Nothing


tree :: Auth (Tree Text)
tree = bin (bin (tip "b") (bin (tip "c") (bin (tip "d") (tip "c")))) (tip "a")

shallowTree :: Auth (Tree Text)
shallowTree = shallow tree

example :: IO ()
example = do
  let proof = snd $ runProver $ fetch [L, R, L] tree
  print $ runVerifier (fetch [L, R, L] shallowTree) proof
  print $ runVerifier (fetch [R] shallowTree) proof

<p align="center">
  <a href="http://www.adjoint.io"><img src="https://www.adjoint.io/assets/img/adjoint-logo@2x.png" width="250"/></a>
</p>


[![CircleCI](https://circleci.com/gh/adjoint-io/auth-adt.svg?style=svg&circle-token=ab946f8d110bbc5e7fe0550d05d414a1fcbcedb0)](https://circleci.com/gh/adjoint-io/auth-adt)

Derive inclusion and membership proofs for arbitrary sum-of-product datatypes
in Haskell using GHC.Generics.

Authenticated Data Structures, Generically
=========================================

Authenticated data structures (ADS) allow untrusted parties (provers) answer queries on a data
structures on behalf of a trusted source (verifier) and provide a compact proof of the computation.
A verifier can then efficiently check the authenticity of the answer.

In their paper "Authenticated Data Structures, Generically"[1], A. Miller et al. present a generic
method to program authenticated operations over any data structure.
They define a well-typed functional programming language, called lambda-auth, whose programs result in code
that is secure under the standard cryptographic assumption of collision-resistant hash functions.

We present an implementation in Haskell of the lambda-auth programming language. In this model of computation,
the prover holds the full ADS of type `Auth T`, which consist of pairs <h<sub>i</sub>, v<sub>i</sub>>
where v<sub>i</sub> is any value of type `T` and h<sub>i</sub> is its digest, i.e. the hash of the
shallow projection of `v`. The verifier only keeps the digest `h` of the authenticated data structure.

An example (more in ExampleAuth.hs):
```haskell
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

-- | Construct an authenticated branch
bin :: Hashable a => Auth (Tree a) -> Auth (Tree a) -> Auth (Tree a)
bin l r = auth (Bin l r)

-- | Construct an authenticated tip
tip :: Hashable a => a -> Auth (Tree a)
tip v = auth $ Tip v

instance Shallow (Tree a) where
    shallow (Tip s) = Tip s
    shallow (Bin l r) = Bin (shallow l) (shallow r)

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

tree :: Auth (Tree Text)
tree = bin (bin (tip "b") (bin (tip "c") (bin (tip "d") (tip "c")))) (tip "a")

-- shallow projection of the tree ( just the root hash )
shallowTree :: Auth (Tree Text)
shallowTree = shallow tree

example :: IO ()
example = do
  let proof = snd $ runProver $ fetch [L, R, L] tree
  print $ runVerifier (fetch [L, R, L] shallowTree) proof
  print $ runVerifier (fetch [R] shallowTree) proof -- returns AuthError because hashes don't match


```

Membership proofs
=================

We also present a method to construct a membership proof for any data structure using GHC.Generics.

An `Authable` typeclass provides two methods: `prove` and `authenticate`.
An untrusted source can invoke the `prove` method to construct a proof of inclusion of an element.
A trusted party can verify that the proof of inclusion comes from the expected data source.

```haskell
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
```

The `authenticate` method in `Authable` generates an authenticated data structure from a non-authenticated data structure.













**References**:
1. A. Miller, M. Hicks, J. Katz, and E. Shi "Authenticated Data Structures, Generically" (https://www.cs.umd.edu/~mwh/papers/gpads.pdf)


License
-------

```
Copyright 2018 Adjoint Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

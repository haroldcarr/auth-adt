{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Authenticated (
  runProver,
  runVerifier,
  Auth(..),
  AuthM,
  auth,
  unAuth,
  Shallow(..),
) where
import Protolude hiding (Hashable)
import Control.Monad.State
import Control.Monad.Except
import GHC.Generics
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..))
import Hash
import qualified Auth


-- |
type ProofStream s = (Seq s)

-- | Errors returned by runVerifier
data AuthError = NoMoreProofElems | MismatchedHash Hash Hash deriving (Show)

-- | An authenticated computation
type AuthM s a = ExceptT AuthError (State (Seq s)) a

-- | Either the value with the hash, or just the hash
data Auth a = WithHash a Hash | OnlyHash Hash deriving (Show, Eq, Functor, Generic1)

instance Hashable a => Hashable (Auth a) where
  toHash (WithHash _ h) = h
  toHash (OnlyHash h) =h

-- | Tag a value with its hash
auth :: Hashable a => a -> Auth a
auth a = WithHash a (toHash a)

-- | When called as Prover, push a shallow version of the auth value to the proof stream
-- and when called as Verifier take a shallow from the proof stream and compare it to the
-- given elements hash
unAuth :: (Shallow a, Hashable a) => Auth a -> AuthM a a
unAuth (WithHash a h) = do
  modify (Seq.|> (shallow a))
  return a
unAuth (OnlyHash hash) = do
  stream <- get
  case Seq.viewl stream of
    EmptyL        -> throwError NoMoreProofElems
    shallow :< xs -> do
      put xs
      let shallowHash = toHash shallow
      if shallowHash == hash
        then return shallow
        else throwError $ MismatchedHash shallowHash hash

class Shallow f where
  shallow :: f -> f
  {-default shallow :: (Generic1 f, GShallow (Rep1 f)) => f -> f-}
  {-shallow = to1 . gshallow . from1-}

instance Shallow (Auth a) where
  shallow (WithHash a h ) = (OnlyHash h)
  shallow h = h

-- |
runProver :: AuthM s a -> (Either AuthError a, ProofStream s)
runProver m = runState (runExceptT m) Seq.empty

-- |
runVerifier :: AuthM s a -> ProofStream s -> Either AuthError a
runVerifier m proof = fst $ runState (runExceptT m) proof

class GShallow f where
    gshallow :: f a -> f a

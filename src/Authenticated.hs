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

import Hash
import qualified Auth

-- | Errors returned by runVerifier
data AuthError = NoMoreProofElems | MismatchedHash Hash Hash deriving (Show)

-- | An authenticated computation
type AuthM s a = ExceptT AuthError (State (Seq s)) a

-- | Either the value with the hash, or just the hash
data Auth a = WithHash a Hash | OnlyHash Hash deriving (Show, Eq, Functor, Generic1)

instance Hashable a => Hashable (Auth a) where
  toHash (WithHash _ h) = h
  toHash (OnlyHash h) =h

instance Auth.Authable Auth where
  prove = undefined
  prove' = undefined
  proveHash = undefined
  authenticate = undefined


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

unAuth (OnlyHash t) = do
  stream <- get
  case Seq.viewl stream of
    Seq.EmptyL  -> throwError NoMoreProofElems
    x Seq.:< xs -> do
      put xs
      if (toHash x) == t
        then return x
        else throwError $ MismatchedHash (toHash x) t

class Shallow f where
  shallow :: f -> f
  {-default shallow :: (Generic1 f, GShallow (Rep1 f)) => f a -> f a-}
  {-shallow = to1 . gshallow . from1-}

instance Shallow (Auth a) where
  shallow (WithHash a h ) = (OnlyHash h)
  shallow h = h

-- |
runProver :: AuthM s a -> (Either AuthError a, Seq s)
runProver m = runState (runExceptT m) Seq.empty

-- |
runVerifier :: AuthM s a -> Seq s -> Either AuthError a
runVerifier m proof = fst $ runState (runExceptT m) proof

class GShallow f where
    gshallow :: f -> f

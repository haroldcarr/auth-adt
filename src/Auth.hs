{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module Auth 
    ( verifyProof
    , fetch
    , Authable(..)
    , Proof
    , ProofElem(..)
    , Side(..)
    ) where

import Protolude hiding (show, Hashable(..))
import Prelude (Show(..))
import Unsafe

import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteArray.Encoding as BA

import GHC.Generics

import Hash

type Proof = [ProofElem]

data Side = L | R deriving (Show, Eq)

data ProofElem = ProofElem
    { side :: Side
    , leftHash :: Hash
    , rightHash :: Hash
    } deriving (Show, Eq)

type AuthTree a = (Hash, Tree a) 
data Tree a
    = Bin (AuthTree a) (AuthTree a)
    | Tip (Maybe a) 
    deriving (Show, Eq)

-- | The verifier begins with the proof stream and
-- just the hash of the authenticated data structure.
-- It first compares this hash to the hash of the first
-- element of the proof stream and checks its side.
-- If L, the left hash of the first element will now become
-- the parent hash that compares with the hash of the second element
-- until it gets to the last element of the proof.
-- The verifier then ...
verifyProof :: Hashable a => Hash -> Proof -> a -> Bool
verifyProof _ [] _ = False
verifyProof parentHash (proof:proofs) a
    | parentHash /= toHash proof = False
    | null proofs = case side proof of
        L -> toHash a == leftHash proof
        R -> toHash a == rightHash proof
    | otherwise = case side proof of
        L -> verifyProof (leftHash proof) proofs a
        R -> verifyProof (rightHash proof) proofs a

fetch :: Hashable a => [Side] -> AuthTree a -> Maybe a
fetch idx authTree = do
    tree <- unAuth authTree
    case (idx, tree) of
        ([], Tip v) -> v
        (L : idx', Bin l _) -> fetch idx' l
        (R : idx', Bin _ r) -> fetch idx' r
        _ -> Nothing
        
-- | unAuth is a private function
unAuth :: Hashable a => AuthTree a -> Maybe (Tree a)
unAuth (h, t@(Bin l r)) = 
    if h == toHash (getHash (fst l) <> getHash (fst r))
        then Just t
        else Nothing
unAuth (h, t@(Tip valueM)) = do
    v <- valueM
    if h == toHash v
        then Just t
        else Nothing

class (Functor f) => Authable f where
    prove :: forall a. (Hashable a, Eq a) => f a -> a -> Proof
    default prove 
        :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f)
        => f a
        -> a
        -> Proof
    prove a item = reverse $ gProve [] (from1 a) item

    prove' :: forall a. (Hashable a, Eq a) => Proof -> f a -> a -> Proof
    default prove'
        :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f)
        => Proof
        -> f a
        -> a
        -> Proof
    prove' path a item = gProve path (from1 a) item

    auth :: forall a. (Hashable a, Eq a) => f a -> AuthTree a
    default auth :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f)
        => f a
        -> AuthTree a
    auth f = gAuth (from1 f)

    proveHash :: forall a. (Hashable a, Eq a) => f a -> Hash
    default proveHash :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f) => f a -> Hash 
    proveHash a = gProveHash (from1 a)

class GAuthable f where
    gProve :: (Hashable a, Eq a) => Proof -> f a -> a -> Proof
    gProveHash :: (Hashable a, Eq a) => f a -> Hash
    gAuth :: (Hashable a, Eq a) => f a -> AuthTree a

instance (Authable f) => GAuthable (Rec1 f) where
    gProve path (Rec1 f) = prove' path f
    gProveHash (Rec1 f) = proveHash f 
    gAuth (Rec1 f) = auth f

instance GAuthable Par1 where
    gProve path (Par1 a) item
        | item == a = path
        | otherwise = []
    gProveHash (Par1 a) = toHash a
    gAuth (Par1 a) = (toHash a, Tip (Just a))

instance GAuthable U1 where
    gProve _ _ _ = []
    gProveHash _  = emptyHash
    gAuth _ = (emptyHash, Tip Nothing)

instance (GAuthable a) => GAuthable (M1 i c a) where
    gProve path (M1 a) = gProve path a
    gProveHash (M1 a) = gProveHash a
    gAuth (M1 a) = gAuth a

instance (GAuthable a, GAuthable b) => GAuthable (a :+: b) where
    gProve path (L1 a) = gProve path a
    gProve path (R1 a) = gProve path a

    gProveHash (L1 a) = gProveHash a
    gProveHash (R1 a) = gProveHash a

    gAuth (L1 a) = gAuth a
    gAuth (R1 a) = gAuth a

instance (GAuthable a, GAuthable b) => GAuthable (a :*: b) where
    gProve path (a :*: b) item 
        =   gProve (ProofElem L (gProveHash a) (gProveHash b) : path) a item 
        ++  gProve (ProofElem R (gProveHash a) (gProveHash b) : path) b item
    
    gProveHash (a :*: b) = 
        toHash (getHash (gProveHash a) <> getHash (gProveHash b))

    gAuth (a :*: b) = 
        (gProveHash (a :*: b)
        , Bin (gProveHash a, snd(gAuth a)) (gProveHash b, snd(gAuth b))
        )

instance Hashable ProofElem where
    toHash (ProofElem s l r) = toHash (getHash l <> getHash r)

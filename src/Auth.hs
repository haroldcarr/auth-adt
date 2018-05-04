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

module Auth where

import Protolude hiding (show, Hashable(..))
import Prelude (Show(..))
import Unsafe

import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteArray.Encoding as BA

import GHC.Generics

newtype Hash = Hash { rawHash :: Digest SHA3_256 }
  deriving (Eq, Ord, Show, BA.ByteArrayAccess)

-- sha256 hash
sha256 :: ByteString -> Hash
sha256 x = Hash (hash x :: Digest SHA3_256)

-- | Size of SHA256 output.
hashSize :: Int
hashSize = 32

-- | All zero hash
emptyHash :: Hash
emptyHash = Hash digest
  where
    digest :: Digest SHA3_256
    digest = unsafeFromJust $ digestFromByteString (BS.replicate hashSize 0)

-- | Use Haskell's Show representation (converted to ByteString) for the
-- contents of the SHA hash.
showHash :: Show a => a -> ByteString
showHash = toS . show

getHash :: Hash -> ByteString
getHash = BA.convert . rawHash

data AuthTree a
  = AuthTip { tipHash :: Hash, tipValue :: a }
  | AuthBin { binHash :: Hash, lAuthTree :: (AuthTree a), rAuthTree :: (AuthTree a) }
  deriving (Show, Functor, Generic)

data Tree a
  = Tip a
  | Bin (Tree a) (Tree a)
  deriving (Show, Eq, Functor, Generic, Generic1, Authable)

toAuthTree :: Hashable a => Tree a -> AuthTree a
toAuthTree (Bin l r) =
    AuthBin (toHash (Bin l r)) (toAuthTree l) (toAuthTree r)
toAuthTree (Tip a) =
    AuthTip (toHash (Tip a)) a

exampleTree :: Tree Int
exampleTree = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)

constructProof :: (Hashable a, Eq a) => AuthTree a -> a -> Proof
constructProof = constructProof' []

type Proof = [ProofElem]

data Side = L | R deriving (Show, Eq)

data ProofElem = ProofElem
    { side :: Side
    , leftHash :: Hash
    , rightHash :: Hash
    } deriving (Show, Eq)

constructProof' :: (Hashable a, Eq a) => Proof -> AuthTree a -> a -> Proof
constructProof' path (AuthTip _ a) item
    | a == item = path
    | otherwise = []
constructProof' path (AuthBin _ l r) item = lpath ++ rpath
    where
        proofItem s n1@AuthBin{} n2@AuthBin{} =
            ProofElem s (binHash l) (binHash r)
        proofItem s n1@AuthBin{} n2@AuthTip{} =
            ProofElem s (binHash l) (tipHash r)
        proofItem s n1@AuthTip{} n2@AuthBin{} =
            ProofElem s (tipHash l) (binHash r)
        proofItem s n1@AuthTip{} n2@AuthTip{} =
            ProofElem s (tipHash l) (tipHash r)
        lpath = constructProof' (proofItem L l r : path) l item
        rpath = constructProof' (proofItem R l r : path) r item

data AuthT a = AuthT Hash a | AuthTEmpty

type Root = Hash

verify :: Hashable a => Root -> Proof -> a -> Bool
verify _ [] _ = False
verify root [a] item =
    if root /= toHash a 
        then False
        else case side a of
            L -> hashItem == leftHash a
            R -> hashItem == rightHash a
    where
        hashItem = toHash item
verify root (a:as) item =
    if root /= toHash a
        then False
        else case side a of
            L -> verify (leftHash a) as item
            R -> verify (rightHash a) as item

class (Functor f) => Authable f where
    prove :: forall a. (Hashable a, Eq a) => f a -> a -> Proof
    default prove :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f) => f a -> a -> Proof
    prove a item = reverse $ gProve [] (from1 a) item

    prove' :: forall a. (Hashable a, Eq a) => Proof -> f a -> a -> Proof
    default prove' 
        :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f) 
        => Proof
        -> f a
        -> a 
        -> Proof
    prove' path a item = gProve path (from1 a) item

    proveHash :: forall a. (Hashable a, Eq a) => f a -> Hash
    default proveHash :: forall a. (Hashable a, Eq a, GAuthable (Rep1 f), Generic1 f) => f a -> Hash 
    proveHash a = gProveHash (from1 a)

class GAuthable f where
    gProve :: (Hashable a, Eq a) => Proof -> f a -> a -> Proof
    gProveHash :: (Hashable a, Eq a) => f a -> Hash

instance (Authable f) => GAuthable (Rec1 f) where
    gProve path (Rec1 f) = prove' path f
    gProveHash (Rec1 f) = proveHash f 

instance GAuthable Par1 where
    gProve path (Par1 a) item 
        | item == a = path
        | otherwise = []
    gProveHash (Par1 a) = toHash a

instance GAuthable U1 where
    gProve _ _ _ = []
    gProveHash _  = emptyHash

instance (GAuthable a) => GAuthable (M1 i c a) where
    gProve path (M1 a) = gProve path a
    gProveHash (M1 a) = gProveHash a

instance (GAuthable a, GAuthable b) => GAuthable (a :+: b) where
    gProve path (L1 a) = gProve path a
    gProve path (R1 a) = gProve path a

    gProveHash (L1 a) = gProveHash a
    gProveHash (R1 a) = gProveHash a

instance (GAuthable a, GAuthable b) => GAuthable (a :*: b) where
    gProve path (a :*: b) item 
        =   gProve (ProofElem L (gProveHash a) (gProveHash b) : path) a item 
        ++  gProve (ProofElem R (gProveHash a) (gProveHash b) : path) b item
    
    gProveHash (a :*: b) = 
        toHash (getHash (gProveHash a) <> getHash (gProveHash b))

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

class Show a => Hashable a where
  toHash :: a -> Hash
  -- | SHA hash ( generic deriving )
  default toHash :: (Generic a, GHashable' (Rep a)) => a -> Hash
  toHash a = gtoHash (from a)

  -- | Covert to string
  toBS :: a -> ByteString
  toBS a = (prefix a) <> (showHash a)

  -- | Prefix the hash input with custom data to distinguish unique types
  prefix :: a -> ByteString
  prefix = const mempty

class GHashable' f where
  gtoHash :: f a -> Hash
  gtoBS :: f a -> ByteString

instance GHashable' U1 where
  gtoHash _ = emptyHash
  gtoBS _ = ""

instance GHashable' (Rec1 f) where
  gtoHash (Rec1 f) = emptyHash
  gtoBS _ = ""

instance (Hashable c) => GHashable' (K1 i c) where
  gtoHash (K1 a) = toHash a
  gtoBS (K1 a) = prefix a <> toBS a

instance (GHashable' a) => GHashable' (M1 i c a) where
  gtoHash (M1 a) = gtoHash a
  gtoBS (M1 a) = gtoBS a

instance (GHashable' a, GHashable' b) => GHashable' (a :+: b) where
  gtoHash (L1 a) = gtoHash a
  gtoHash (R1 a) = gtoHash a

  gtoBS (L1 a) = gtoBS a
  gtoBS (R1 a) = gtoBS a

instance (GHashable' a, GHashable' b) => GHashable' (a :*: b) where
  gtoHash (a :*: b) = toHash (gtoBS a <> gtoBS b)
  gtoBS (a :*: b) = gtoBS a <> gtoBS b

-- Instances

instance Hashable ByteString where
  toHash = Hash . hash
  toBS = identity

instance Hashable Int where
  toHash = Hash . hash . showHash

instance Hashable Bool where
  toHash = Hash . hash . showHash

instance (Hashable a) => Hashable (Tree a) where
  toHash (Bin l r) = toHash (getHash (toHash l) <> getHash (toHash r))
  toHash (Tip a) = toHash a

instance Hashable ProofElem where
  toHash (ProofElem s l r) = toHash (getHash l <> getHash r)

instance (Hashable a, Hashable b) => Hashable (a,b)

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  toHash (Left x) = toHash x
  toHash (Right x) = toHash x

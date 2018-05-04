{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveAnyClass #-}

module Auth where

import Protolude hiding (show, Hashable(..))
import Prelude (Show(..))
import Unsafe

import Crypto.Hash
import Crypto.Hash.Algorithms
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteArray.Encoding as BA

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
  deriving (Show, Eq, Functor, Generic, Authable)

-- hashTree :: Hashable a => Tree a -> Hash
-- hashTree (Bin l r) = toHash (getHash (hashTree l) <> getHash (hashTree r))
-- hashTree (Tip a) = toHash a

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

data ProofElem
    = ProofElem 
        { side :: Side
        , leftHash :: Hash
        , rightHash :: Hash 
        }
    | ProofLeaf
        { leafHash :: Hash }
    deriving (Show, Eq)

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

class (Hashable a, Eq a) => Authable a where
--    prove :: a -> Proof
--    default prove :: (Generic a, GAuthable (Rep a)) => a -> Proof
--    prove a = gProve (from a)
    auth :: a -> AuthTree [Char] 
    default auth :: (Generic a, GAuthable (Rep a)) => a -> AuthTree [Char]
    auth a = gAuth (from a)

class GAuthable f where
--    gProve :: f a -> Proof
    gAuth :: f a -> AuthTree [Char]

instance GAuthable U1 where
--    gProve _ = [ProofLeaf emptyHash]
    gAuth _ = AuthTip emptyHash mempty

instance (Hashable a) => GAuthable (K1 i a) where
--    gProve (K1 a) = [ProofLeaf (toHash a)]
    gAuth (K1 a) = AuthTip (toHash a) mempty

instance (GAuthable a) => GAuthable (M1 i c a) where
--    gProve (M1 a) = gProve a
    gAuth (M1 a) = gAuth a

instance (GAuthable a, GAuthable b) => GAuthable (a :+: b) where
--    gProve (L1 a) = gProve a
--    gProve (R1 a) = gProve a
    gAuth (L1 a) = gAuth a
    gAuth (R1 a) = gAuth a

instance (GAuthable a, GAuthable b) => GAuthable (a :*: b) where
--    gProve(a :*: b) = gProve a ++ gProve b 
    -- [ProofElem L (head : gProve a : gProve b]
    gAuth (a :*: b) = AuthBin emptyHash (gAuth a) (gAuth b)
    -- (toHash (getHash (toHash l) <> getHash (toHash r))) 
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

instance (Hashable a, Hashable b) => Hashable (a,b)

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  toHash (Left x) = toHash x
  toHash (Right x) = toHash x

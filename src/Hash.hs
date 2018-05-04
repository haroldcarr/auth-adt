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

module Hash
    ( Hash(..)
    , emptyHash
    , getHash
    , showHash
    , Hashable(..)
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

newtype Hash = Hash { rawHash :: Digest SHA3_256 }
  deriving (Eq, Ord, Show)

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

instance (Hashable a, Hashable b) => Hashable (a,b)

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  toHash (Left x) = toHash x
  toHash (Right x) = toHash x

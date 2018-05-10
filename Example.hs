{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

import Protolude hiding (Hashable, show)

import Prelude (Show(..))
import Auth hiding (Side(..))
import Hash

{-data Tree a-}
  {-= Tip a-}
  {-| Bin (Tree a) (Tree a)-}
  {-deriving (Show, Eq, Functor, Generic, Generic1, Authable, Hashable)-}

{-exampleTree :: Tree Int-}
{-exampleTree = Bin (Bin (Tip 3) (Bin (Bin (Tip 2) (Tip 5)) (Tip 8))) (Tip 1)-}

----------------------------------------
-- Non generic example
----------------------------------------

{-data AuthTree a-}

  {-= AuthTip { tipHash :: Hash, tipValue :: a }-}
  {-| AuthBin { binHash :: Hash, lAuthTree :: (AuthTree a), rAuthTree :: (AuthTree a) }-}
  {-deriving (Show, Functor, Generic)-}

{-toAuthTree :: Hashable a => Tree a -> AuthTree a-}
{-toAuthTree (Bin l r) =-}
    {-AuthBin (toHash (Bin l r)) (toAuthTree l) (toAuthTree r)-}
{-toAuthTree (Tip a) =-}
    {-AuthTip (toHash (Tip a)) a-}

{-constructProof :: (Hashable a, Eq a) => AuthTree a -> a -> Proof-}
{-constructProof = constructProof' []-}

{-constructProof' :: (Hashable a, Eq a) => Proof -> AuthTree a -> a -> Proof-}
{-constructProof' path (AuthTip _ a) item-}
    {-| a == item = path-}
    {-| otherwise = []-}
{-constructProof' path (AuthBin _ l r) item = lpath ++ rpath-}
    {-where-}
        {-proofItem s n1@AuthBin{} n2@AuthBin{} =-}
            {-ProofElem s (binHash l) (binHash r)-}
        {-proofItem s n1@AuthBin{} n2@AuthTip{} =-}
            {-ProofElem s (binHash l) (tipHash r)-}
        {-proofItem s n1@AuthTip{} n2@AuthBin{} =-}
            {-ProofElem s (tipHash l) (binHash r)-}
        {-proofItem s n1@AuthTip{} n2@AuthTip{} =-}
            {-ProofElem s (tipHash l) (tipHash r)-}
        {-lpath = constructProof' (proofItem L l r : path) l item-}
        {-rpath = constructProof' (proofItem R l r : path) r item-}



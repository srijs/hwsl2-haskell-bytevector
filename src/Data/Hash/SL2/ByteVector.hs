{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Hash.SL2.ByteVector
  ( ByteVector()
    -- * Construction
  , singleton, fromList
    -- * Composition
  , empty, append
  , cons, snoc
    -- * Introspection
  , null, hash, length
    -- * Deconstruction
  , ViewL(..), viewl, viewl1
  , ViewR(..), viewr, viewr1
  , splitBefore, splitAt
    -- * Transformation
  , reverse, map
    -- * Reduction
  , foldl, foldr, foldl', foldr'
  ) where

import Prelude hiding (concat, null, reverse, map, length, foldl, foldr, splitAt)

import Data.Coerce
import qualified Data.Foldable as Foldable
import Data.Monoid

import Data.Hash.SL2 (Hash)
import qualified Data.Hash.SL2 as Hash
import Data.Hash.SL2.Chunk (Chunk(..), fromByteString)

import Data.FingerTree (FingerTree, (<|), (|>))
import qualified Data.FingerTree as FingerTree

import qualified Data.ByteString as ByteString

data Measure = Measure
  { getHash :: !Hash
  , getLength :: !Integer
  }

instance Monoid Measure where
  mempty = Measure mempty 0
  mappend a b = Measure (getHash a <> getHash b) (getLength a + getLength b)
  mconcat as = Measure (mconcat $ fmap getHash as) (sum $ fmap getLength as)

newtype MeasuredChunk = MeasuredChunk
  { getChunk :: Chunk
  } deriving (Eq, Ord)

instance FingerTree.Measured Measure MeasuredChunk where
  measure b = Measure (getChunkHash $ coerce b) (fromIntegral . ByteString.length . getChunkBytes $ coerce b)

newtype ByteVector = ByteVector
  { getTree :: FingerTree Measure MeasuredChunk
  } deriving (Monoid)

instance Eq ByteVector where
  a == b = getHash (FingerTree.measure $ getTree a) == getHash (FingerTree.measure $ getTree b)

instance Ord ByteVector where
  compare a b = compare (getHash . FingerTree.measure $ getTree a) (getHash . FingerTree.measure $ getTree b)

-- | /O(1)/ The empty vector. Alias for 'mempty'.
empty :: ByteVector
empty = ByteVector mempty

-- | /O(1)/ Creates a vector from a single chunk.
singleton :: Chunk -> ByteVector
singleton = ByteVector . FingerTree.singleton . coerce

-- | /O(n)/ Creates a vector from a list of chunks.
fromList :: [Chunk] -> ByteVector
fromList = ByteVector . FingerTree.fromList . fmap coerce

-- | /O(n)/ Concatenates two vectors together. Alias for 'mappend'.
append :: ByteVector -> ByteVector -> ByteVector
append a b = ByteVector $ getTree a <> getTree b

-- | /O(1)/ Adds a chunk to the left end of the vector.
cons :: Chunk -> ByteVector -> ByteVector
cons c v = ByteVector $ coerce c <| coerce v

-- | /O(1)/ Adds a chunk to the right end of the vector.
snoc :: ByteVector -> Chunk -> ByteVector
snoc v c = ByteVector $ coerce v |> coerce c

-- | /O(1)/ Is this the empty vector?
null :: ByteVector -> Bool
null v = getHash measure == Hash.unit && getLength measure == 0
  where measure = FingerTree.measure (getTree v)

-- | /O(1)/ Returns the hash of the vector.
hash :: ByteVector -> Hash
hash = getHash . FingerTree.measure . getTree

-- | /O(1)/ Returns the number of bytes in the vector.
length :: ByteVector -> Integer
length = getLength . FingerTree.measure . getTree

data ViewL = EmptyL | MostL Chunk ByteVector
  deriving (Eq, Ord)

-- | /O(1)/ Creates a view of the left end of the vector.
viewl :: ByteVector -> ViewL
viewl v = case FingerTree.viewl (getTree v) of
  FingerTree.EmptyL -> EmptyL
  most FingerTree.:< rest -> MostL (coerce most) (coerce rest)

-- | /O(1)/ Creates a view of the left end of the vector.
--          The single chunk is guaranteed to never be empty.
viewl1 :: ByteVector -> ViewL
viewl1 v = case viewl v of
  EmptyL -> EmptyL
  MostL most rest | mempty == most -> viewl1 rest
                  | otherwise -> MostL most rest

data ViewR = EmptyR | MostR ByteVector Chunk
  deriving (Eq, Ord)

-- | /O(1)/ Creates a view of the right end of the vector.
viewr :: ByteVector -> ViewR
viewr v = case FingerTree.viewr (getTree v) of
  FingerTree.EmptyR -> EmptyR
  rest FingerTree.:> most -> MostR (coerce rest) (coerce most)

-- | /O(1)/ Creates a view of the right end of the vector.
--          The single chunk is guaranteed to never be empty.
viewr1 :: ByteVector -> ViewR
viewr1 v = case viewr v of
  EmptyR -> EmptyR
  MostR rest most | mempty == most -> viewr1 rest
                  | otherwise -> MostR rest most

-- | /O(log(min(i,n-i)))/ Splits the vector at the chunk where the accumulated
--   length equals the provided integer /i/.
--   The resulting left side has a length smaller /i/.
splitBefore :: Integer -> ByteVector -> (ByteVector, ByteVector)
splitBefore i v = (coerce left, coerce right)
  where (left, right) = FingerTree.split (\m -> getLength m >= i) (getTree v)

-- | /O(log(min(i,n-i)))/ Splits the vector at the byte where the accumulated
--   length equals the provided integer /i/.
--   The resulting left side has a length of /min(i,n)/.
--
--   This is potentially less efficient than 'splitBefore' because it has to
--   re-hash parts of the vector.
splitAt :: Integer -> ByteVector -> (ByteVector, ByteVector)
splitAt i v = splitView (viewl right)
  where (left, right) = splitBefore i v
        splitView EmptyL = (left, right)
        splitView (MostL most rest) = (snoc left (fromByteString left'), cons (fromByteString right') rest)
          where (left', right') = ByteString.splitAt (fromIntegral $ i - length left) . getChunkBytes $ coerce most

-- | /O(n)/ Folds the vector from left to right.
foldl :: (a -> Chunk -> a) -> a -> ByteVector -> a
foldl f a v = Foldable.foldl (\a' -> f a' . getChunk) a (getTree v)

-- | /O(n)/ Folds the vector from right to left.
foldr :: (Chunk -> a -> a) -> a -> ByteVector -> a
foldr f a v = Foldable.foldr (\c a' -> f (getChunk c) a') a (getTree v)

-- | /O(n)/ Folds the vector from left to right, with strict application.
foldl' :: (a -> Chunk -> a) -> a -> ByteVector -> a
foldl' f a v = Foldable.foldl' (\a' -> f a' . getChunk) a (getTree v)

-- | /O(n)/ Folds the vector from right to left, with strict application.
foldr' :: (Chunk -> a -> a) -> a -> ByteVector -> a
foldr' f a v = Foldable.foldr' (\c a' -> f (getChunk c) a') a (getTree v)

-- | /O(n)/ Applies a function to every chunk in the vector.
map :: (Chunk -> Chunk) -> ByteVector -> ByteVector
map f v = ByteVector (FingerTree.fmap' (coerce . f . getChunk) $ coerce v)

-- | /O(n)/ Reverses the vector.
reverse :: ByteVector -> ByteVector
reverse v = ByteVector $ FingerTree.reverse $ FingerTree.fmap' (coerce . reverseChunk . coerce) (getTree v)
  where reverseChunk c = let r = ByteString.reverse (getChunkBytes c) in Chunk (Hash.hash r) r

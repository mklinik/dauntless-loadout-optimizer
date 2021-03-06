module Histogram where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable
import Data.List

newtype Histogram a = Histogram (Map a Int)

instance Ord a => Semigroup (Histogram a) where
  (Histogram m1) <> (Histogram m2) = Histogram $ Map.unionWith (+) m1 m2

instance Ord a => Monoid (Histogram a) where
  mempty = Histogram Map.empty

instance (Show a, Ord a) => Show (Histogram a) where
  show (Histogram m) = concat $ intersperse " " [ show k ++ ":" ++ show v | (k,v) <- sort $ Map.toList m, v /= 0]

inc :: Ord a => a -> Histogram a -> Histogram a
inc a (Histogram m) = Histogram $ Map.alter (maybe (Just 1) (Just . (\n -> n+1))) a m

dec :: Ord a => a -> Histogram a -> Histogram a
dec a (Histogram m) = Histogram $ Map.alter (maybe (Just (-1)) (Just . (\n -> n-1))) a m

num :: Ord a => a -> Histogram a -> Int
num a (Histogram m) = Map.findWithDefault 0 a m

absSum :: Ord a => Histogram a -> Int
absSum (Histogram m) = sum $ map abs $ Map.elems m

maxElem :: Ord a => Histogram a -> Int
maxElem (Histogram m) =
  if null m
    then 0
    else maximum m

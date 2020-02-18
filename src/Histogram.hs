module Histogram where

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

newtype Histogram a = Histogram (Map a Int)

instance Show a => Show (Histogram a) where
  show (Histogram m) = concat $ intersperse " " [ show k ++ ":" ++ show v | (k,v) <- Map.toList m]

inc :: Ord a => a -> Histogram a -> Histogram a
inc a (Histogram m) = Histogram $ Map.alter (maybe (Just 1) (Just . (\n -> n+1))) a m

dec :: Ord a => a -> Histogram a -> Histogram a
dec a (Histogram m) = Histogram $ Map.alter (maybe (Just (-1)) (Just . (\n -> n-1))) a m

num :: Ord a => Histogram a -> a -> Int
num (Histogram m) a = Map.findWithDefault 0 a m

absSum :: Ord a => Histogram a -> Int
absSum (Histogram m) = sum $ map abs $ Map.elems m

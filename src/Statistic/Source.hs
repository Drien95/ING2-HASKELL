{- |
  Module : Statistic.Source
  Description : Some utility functions for sources (input messages)
  Maintainer : ???
-}

module Statistic.Source(occurrences, entropy, orderedCounts) where

import Data.Map (Map, insertWith, elems, toList, empty)
import Data.List (sortOn)

-- | The map giving occurrences of each symbol in the source
occurrences :: Ord a => [a] -> Map a Int
occurrences source = foldl (\m x -> insertWith (+) x 1 m) empty source 

-- | SHANNON entropy of source
entropy :: Ord a => [a] -> Double
entropy source = -(foldl (\acc x -> ((fromIntegral x)/l) * logBase 2 ((fromIntegral x)/l) + acc) 0.0 (elems (occurrences source)))
  where l = fromIntegral (length source)


-- | List of occurrences ordered by count
orderedCounts :: Ord a => [a] -> [(a, Int)]
orderedCounts source = sortOn (\tuple -> snd tuple) (toList (occurrences source))


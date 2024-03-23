{- |
  Module : Statistic.ShannonFano
  Description : A module containing specifics for the Shannon-Fano compression method
  Maintainer : ???
-}
module Statistic.ShannonFano(tree) where

import Statistic.EncodingTree

import Statistic.Source

-- | Shannon-Fano tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree [] = Nothing
tree source = Just (treeAux (reverse (orderedCounts source)))
  where treeAux [(a,b)] = EncodingLeaf b a
        treeAux list = EncodingNode total (treeAux (fst splittedList)) (treeAux (snd splittedList))
          where 
            total = sum (map (snd) list)
            splittedList = split list 


-- | Determines which number is the closest to target between x and y
closestTo :: (Num a, Ord a) => a -> a -> a -> a
closestTo x y target
    | abs (x - target) <= abs (y - target) = x
    | otherwise = y

-- | Splits a list of orderedOcurrences in the most balanced two subLists 
split :: Ord a => [(a, Int)] -> ([(a, Int)], [(a, Int)])
split l = splitAt (splitInd 0 0.0 l) l
  where 
    md :: Double
    md = fromIntegral (sum (map (snd) l)) / 2.0
    splitInd i _ [] = i  
    splitInd i acc (x:xs) 
      | v + acc == md  = i+1
      | v + acc > md = if (closestTo acc (v+acc)  md) == acc then i else i+1
      | otherwise = splitInd (i+1) (v+acc) xs
        where 
          v = fromIntegral (snd x)
          



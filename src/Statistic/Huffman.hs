module Statistic.Huffman (tree) where

import Statistic.EncodingTree

import Statistic.Source

import Data.List (sortOn)

makeLeaves :: Ord a => [a] -> [EncodingTree a]
makeLeaves source = map (\(sym, count) -> EncodingLeaf count sym) (orderedCounts source)

huffmanTree :: Ord a => [EncodingTree a] -> Maybe (EncodingTree a)
huffmanTree [] = Nothing
huffmanTree [t] = Just t
huffmanTree (x:y:ts) = huffmanTree (insertOrdered newTree ts)
  where
    newTree = mergeTrees x y
    mergeTrees (EncodingLeaf count1 sym1) (EncodingLeaf count2 sym2) = EncodingNode (count1 + count2) (EncodingLeaf count1 sym1) (EncodingLeaf count2 sym2)
    mergeTrees (EncodingLeaf count1 sym1) (EncodingNode count2 left right) = EncodingNode (count1 + count2) (EncodingLeaf count1 sym1) (EncodingNode count2 left right)
    mergeTrees (EncodingNode count1 left right) (EncodingLeaf count2 sym2) = EncodingNode (count1 + count2) (EncodingNode count1 left right) (EncodingLeaf count2 sym2)
    mergeTrees (EncodingNode count1 left1 right1) (EncodingNode count2 left2 right2) = EncodingNode (count1 + count2) (EncodingNode count1 left1 right1) (EncodingNode count2 left2 right2)
    insertOrdered tree trees = sortOn treeWeight (tree:trees)
    treeWeight (EncodingLeaf count _) = count
    treeWeight (EncodingNode count _ _) = count

-- | Huffman tree generation
tree :: Ord a => [a] -> Maybe (EncodingTree a)
tree source = huffmanTree (makeLeaves source)

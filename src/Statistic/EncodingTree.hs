{- |
  Module : Statistic.EncodingTree
  Description : A module representing a binary tree for binary encoding
  Maintainer : ???
-}
module Statistic.EncodingTree(EncodingTree(..), isLeaf, count, has, encode, decodeOnce, decode, meanLength, compress, uncompress) where

import Statistic.Bit

data EncodingTree a = EncodingNode Int (EncodingTree a) (EncodingTree a)
                    | EncodingLeaf Int a
  deriving (Eq, Show)

-- | Is the encoding a mere leaf ?
isLeaf :: EncodingTree a -> Bool
isLeaf (EncodingLeaf _ _) = True
isLeaf  _                 = False

-- | The length of the underlying source
count :: EncodingTree a -> Int
count (EncodingLeaf cnt _  ) = cnt
count (EncodingNode cnt _ _) = cnt

-- | Search for symbol in encoding tree
has :: Eq a => EncodingTree a -> a -> Bool
has (EncodingLeaf _ symbol) s = symbol == s
has (EncodingNode _ left right) s = left `has` s || right `has` s

-- | Computes the binary code of symbol using encoding tree
-- If computation is not possible, returns `Nothing`.
encode :: Eq a => EncodingTree a -> a -> Maybe [Bit] 
encode (EncodingLeaf _ symbol) s
    | symbol == s = Just [] -- if we're on the leaf, we're done
    | otherwise        = Nothing
encode (EncodingNode _ left right) s -- if we're on a node, we need to go deeper
    | left `has` s  = fmap (Zero :) (encode left s) -- if the symbol is in the left subtree, we add a 0 and continue
    | right `has` s = fmap (One :) (encode right s) -- if the symbol is in the right subtree, we add a 1 and continue
    | otherwise          = Nothing
encode _ _ = Nothing


-- | Computes the first symbol from list of bits using encoding tree and also returns the list of bits still to process
-- If computation is not possible, returns `Nothing`.
decodeOnce :: EncodingTree a -> [Bit] -> Maybe (a, [Bit])
decodeOnce (EncodingLeaf _ symbol) bits = Just (symbol, bits) -- if we're on the leaf, we're done returning the symbol and the rest of the bits
decodeOnce (EncodingNode _ left right) (bit:bits) -- if we're on a node, we need to go deeper, so we take the first bit and continue
    | bit == Zero = decodeOnce left bits -- if the bit is 0, we go left
    | bit == One  = decodeOnce right bits -- if the bit is 1, we go right
decodeOnce _ _ = Nothing

-- | Computes list of symbols from list of bits using encoding tree
decode :: EncodingTree a -> [Bit] -> Maybe [a]
decode _ [] = Just []
decode tree bits = case decodeOnce tree bits of
  Just (symbol, restBits) -> case decode tree restBits of
    Just symbols -> Just (symbol : symbols)
    Nothing      -> Nothing
  Nothing -> Nothing

-- | Calculate the total number of symbols in the encoding tree
totalLength :: EncodingTree a -> Int
totalLength (EncodingLeaf c _) = c
totalLength (EncodingNode c left right) = c + totalLength left + totalLength right

-- | Mean length of the binary encoding
meanLength :: EncodingTree a -> Double
meanLength tree = fromIntegral (countSymbols tree) / fromIntegral (totalLength tree)
  where
    countSymbols (EncodingLeaf c _) = c
    countSymbols (EncodingNode c _ _) = c


-- | Compress method using a function generating encoding tree and also returns generated encoding tree
compress :: Eq a => ([a] -> Maybe (EncodingTree a)) -> [a] -> (Maybe (EncodingTree a), [Bit])
compress _ _ = undefined -- TODO

-- | Uncompress method using previously generated encoding tree
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: (Maybe (EncodingTree a), [Bit]) -> Maybe [a]
uncompress _ = undefined -- TODO



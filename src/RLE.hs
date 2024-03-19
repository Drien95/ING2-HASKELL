{- |
  Module      : RLE
  Description : An implementation of the run-length encoding method
  Maintainer  : ???
-}
module RLE (compress, uncompress) where

-- | RLE compress method
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress (x:xs) = let (packed, rest) = span (== x) xs
                      count = 1 + length packed
                  in (x, count) : compress rest

-- | RLE uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(a, Int)] -> Maybe [a]
uncompress [] = Just []
uncompress ((x, count):xs)
  | count <= 0 = Nothing
  | otherwise = case uncompress xs of
                  Nothing -> Nothing
                  Just rest -> Just (replicate count x ++ rest)

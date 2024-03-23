{- |
  Module      : LZ.LZ78
  Description : An implementation of LZ78 method
  Maintainer  : ???
-}
module LZ.LZ78 (compress, uncompress) where

import LZ.Dictionaries (empty, zeroAsChar)

-- | LZ78 compress method
compress :: String -> [(Int, Char)]
compress input = compressAux input 1 empty

compressAux :: String -> Int -> [String] -> [(Int, Char)]
compressAux [] _ _ = []
compressAux (c:cs) nextIndex dict =
    let (matchedString, rest) = longestMatch [c] cs dict
    in case lookup matchedString (zip dict [0..]) of
        Just index -> case rest of
            r:rs -> (index, r) : compressAux rs (nextIndex + 1) (dict ++ [matchedString ++ [r]])
            [] -> [(index, zeroAsChar)] 
        Nothing -> case rest of
            r:rs -> (0, c) : compressAux cs (nextIndex + 1) (dict ++ [[c]])
            [] -> [(0, c)]

longestMatch :: String -> String -> [String] -> (String, String)
longestMatch current (c:cs) dict =
    let next = current ++ [c]
    in if null cs || next `notElem` dict then (current, c:cs)
       else longestMatch next cs dict
longestMatch current [] _ = (current, [])


-- | LZ78 uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [(Int, Char)] -> Maybe String
uncompress compressed = decompressAux compressed 1 empty
  where
    decompressAux :: [(Int, Char)] -> Int -> [String] -> Maybe String
    decompressAux [] _ _ = Just ""
    decompressAux ((index, char):cs) dictIndex dict =
      let newString = if index == 0 then [char] else (dict !! index) ++ [char]
          newDict = dict ++ [newString]
      in if index < dictIndex
         then fmap (newString ++) (decompressAux cs (dictIndex + 1) newDict)
         else Nothing

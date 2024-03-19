{- |
  Module      : LZ.LZW
  Description : An implementation of LZW method
  Maintainer  : ???
-}
module LZ.LZW (compress, uncompress) where

import LZ.Dictionaries (ascii)

-- | LZW compress Method
compress :: String -> [Int]
compress [] = []
compress input = compress' input asciiDict 256 ""
  where
    -- Dictionnaire ASCII initial
    asciiDict :: [(String, Int)]
    asciiDict = [(c:[], fromEnum c) | c <- ['\0'..'\255']]

    -- Fonction de compression auxiliaire
    compress' :: String -> [(String, Int)] -> Int -> String -> [Int]
    compress' [] dict _ currWord =
      case lookup currWord dict of
        Just idx -> [idx]
        Nothing  -> error "Invalid input: empty string"
    compress' (x:xs) dict nextIndex currWord =
      case lookup (currWord ++ [x]) dict of
        Just idx -> compress' xs dict nextIndex (currWord ++ [x])
        Nothing  -> let newDict = dict ++ [(currWord ++ [x], nextIndex)]
                    in idx : compress' xs newDict (nextIndex + 1) [x]
                      where
                        Just idx = lookup currWord dict

      
-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress input = uncompress' input asciiDict
  where
    -- Dictionnaire ASCII initial
    asciiDict :: [String]
    asciiDict = map (:[]) ['\0'..'\255']

    -- Fonction de décompression auxiliaire
    uncompress' :: [Int] -> [String] -> Maybe String
    uncompress' [] _ = Just ""
    uncompress' (x:xs) dict
      | x >= 0 && x < length dict = do
          let word = dict !! x
          case xs of
            [] -> return word
            (y:ys) ->
              let nextWord = if y < length dict
                                then dict !! y
                                else if y == length dict
                                       then word ++ [head word]
                                       else error "Invalid compressed data"
              in (word ++) <$> uncompress' xs (dict ++ [word ++ [head nextWord]])
      | otherwise = Nothing

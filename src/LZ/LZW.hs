module LZ.LZW (compress, uncompress) where

import LZ.Dictionaries (ascii, Dictionary)
import Data.List (elemIndex)
import Data.Maybe


-- | LZW compress Method
compress :: String -> [Int]
compress = compress' ascii
    where
        compress' :: Dictionary -> String -> [Int]
        compress' _ [] = []
        compress' dict input = case longestPrefix dict input of
            (prefix, rest) -> getIndex prefix dict : compress' (addToDict (prefix ++ [head rest]) dict) rest

        longestPrefix :: Dictionary -> String -> (String, String)
        longestPrefix dict input = go "" input
            where
                go :: String -> String -> (String, String)
                go acc [] = (acc, [])
                go acc (x:xs)
                    | (acc ++ [x]) `elem` dict = go (acc ++ [x]) xs
                    | otherwise = (acc, x:xs)


        addToDict :: String -> Dictionary -> Dictionary
        addToDict s dict
            | s `elem` dict = dict
            | otherwise = dict ++ [s]

        getIndex :: String -> Dictionary -> Int
        getIndex s dict = case s `elemIndex` dict of
            Just index -> index
            Nothing -> error "Index not found in dictionary"


-- | LZW uncompress method
-- If input cannot be uncompressed, returns `Nothing`
uncompress :: [Int] -> Maybe String
uncompress [] = Nothing
uncompress code = uncompress' code 0 ascii (Just [])
  where
    uncompress' :: [Int] -> Int -> Dictionary -> Maybe String -> Maybe String
    uncompress' [] _ _ acc = acc
    uncompress' (index:code) preIndex dict acc
      | index < 0 || length dict < index || isNothing acc || (length dict == index && length (fromJust acc) == 0) = Nothing
      | otherwise = uncompress' code index newDict (Just (maybeAcc ++ char))
      where
        maybeAcc = fromJust acc
        char = newDict !! index
        preChar = dict !! preIndex
        newDict = if length maybeAcc == 0
                  then dict
                  else if index < (length dict)
                       then dict ++ [(preChar ++ [head char])]
                       else dict ++ [(preChar ++ [head preChar])]


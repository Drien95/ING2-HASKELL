module LZ.LZW (compress, uncompress) where

import LZ.Dictionaries (ascii, Dictionary)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

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
uncompress xs = uncompress' ascii xs
    where
        uncompress' :: Dictionary -> [Int] -> Maybe String
        uncompress' _ [] = Just []
        uncompress' dict (x:xs) = case decode x dict of
            Just entry -> case uncompressRest dict entry xs of
                Just rest -> fmap (entry ++) (uncompress' (addToDict (entry ++ if null rest then "" else [head rest]) dict) xs)
                Nothing -> Nothing
            Nothing -> error "Index not found in dictionary"

        decode :: Int -> Dictionary -> Maybe String
        decode index dict
            | index < length dict = Just (dict !! index)
            | otherwise = Nothing

        uncompressRest :: Dictionary -> String -> [Int] -> Maybe String
        uncompressRest _ _ [] = Just []
        uncompressRest dict w (y:ys) = case decode y dict of
            Just entry -> Just entry
            Nothing -> case decode (length dict) dict of
                Just newEntry -> Just newEntry
                Nothing -> Nothing

        addToDict :: String -> Dictionary -> Dictionary
        addToDict s d = if s `elem` d then d else d ++ [s]

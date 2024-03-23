import Statistic.EncodingTree
import Statistic.Bit
import Statistic.Huffman

import Data.Function (on)

exampleSource :: String
exampleSource = "AABBC"


-- list of bits to base 10
bitsToBase10 :: [Bit] -> Int
bitsToBase10 [] = 0
bitsToBase10 (bit:bits) = fromEnum bit * 2^(length bits) + bitsToBase10 bits

-- Testez la fonction `decode` avec une chaîne de bits plus longue
main :: IO ()
main = do
 -- Générez l'arbre Shannon-Fano à partir de la source
    let shannonTree = tree exampleSource

    -- Compressez la source en utilisant l'arbre Shannon-Fano généré
    let shannonCompress = compress tree exampleSource

    let shannonDecompress = uncompress shannonCompress
    
    let sourceSize = length exampleSource  -- Taille de la chaîne d'origine
    
    case shannonCompress of
        (Just encodingTree, encodedSymbols) -> do
            let encodedBase10 = bitsToBase10 encodedSymbols  -- Conversion des bits en base 10
            let compressedSizeBits = length encodedSymbols  -- Taille de la chaîne compressée en bits
            let compressedSizeOctets = ceiling ( fromIntegral compressedSizeBits  /8)  
            let totlength = totalLength encodingTree
            let test = meanLength encodingTree
            putStrLn $ "Arbre de huffman : " ++ show encodingTree ++ "\n" ++
                        "Symboles d'origine : " ++ show encodedSymbols ++ "\n" ++
                       "Taille encodés base 10 : " ++ show encodedBase10 ++ "\n" ++
                       "Taille de la chaîne d'origine : " ++ show sourceSize ++ " caractères" ++ "\n" ++
                       "Taille de la chaîne compressée : " ++ show compressedSizeBits ++ " bits" ++ "\n" ++
                       "Taille de la chaîne compressée : " ++ show compressedSizeOctets ++ " octets" ++ "\n" ++
                       "meanLength : " ++ show test ++ "\n" ++
                       "totalLength : " ++ show totlength
        (Nothing, _) -> putStrLn "Impossible de créer l'arbre Shannon-Fano"

    case shannonDecompress of
        Just decodedSymbols -> putStrLn $ "Symboles décodés : " ++ show decodedSymbols
        Nothing -> putStrLn "Impossible de décoder les symboles"
        

    {-- -- Construction de l'arbre de Huffman
    let huffmanTree = tree exampleSource
    
    -- Affichage de l'arbre de Huffman
    putStrLn "Arbre de Huffman :"
    case huffmanTree of
        Just tree -> print tree
        Nothing   -> putStrLn "Impossible de créer l'arbre de Huffman." --}
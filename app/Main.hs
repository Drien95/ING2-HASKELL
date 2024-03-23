import Statistic.EncodingTree
import Statistic.Bit
import Statistic.Huffman

exampleSource :: String
exampleSource = "AABBCE"

-- Testez la fonction `decode` avec une chaîne de bits plus longue
main :: IO ()
main = do
   {--  -- Générez l'arbre Shannon-Fano à partir de la source
    let shannonTree = tree exampleSource

    -- Compressez la source en utilisant l'arbre Shannon-Fano généré
    let shannonCompress = compress tree exampleSource

    let shannonDecompress = uncompress shannonCompress
    
    -- Affichez le résultat
    case shannonCompress of
        (Just encodingTree, encodedSymbols) -> putStrLn $ "Arbre Shannon-Fano : " ++ show encodingTree ++ "\n" ++ "Symboles encodés : " ++ show encodedSymbols
        (Nothing, _) -> putStrLn "Impossible de créer l'arbre Shannon-Fano"

    case shannonDecompress of
        Just decodedSymbols -> putStrLn $ "Symboles décodés : " ++ show decodedSymbols
        Nothing -> putStrLn "Impossible de décoder les symboles"
        --}

    -- Construction de l'arbre de Huffman
    let huffmanTree = tree exampleSource
    
    -- Affichage de l'arbre de Huffman
    putStrLn "Arbre de Huffman :"
    case huffmanTree of
        Just tree -> print tree
        Nothing   -> putStrLn "Impossible de créer l'arbre de Huffman."
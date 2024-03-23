import Statistic.EncodingTree
import Statistic.Bit
import Statistic.ShannonFano

exampleSource :: String
exampleSource = "AAAABBBCCD"

-- Testez la fonction `decode` avec une chaîne de bits plus longue
main :: IO ()
main = do
     -- Générez l'arbre Shannon-Fano à partir de la source
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
import Statistic.EncodingTree
import Statistic.Bit

-- Définissez un exemple d'arbre d'encodage pour tester
exampleTree :: EncodingTree Char
exampleTree =
    EncodingNode 10
        (EncodingNode 5 (EncodingLeaf 2 'A') (EncodingLeaf 3 'B'))
        (EncodingLeaf 5 'C')

-- Testez la fonction `decode` avec une chaîne de bits plus longue
main :: IO ()
main = do
    putStrLn "Testing decode with a longer bit string:"
    let bits = [One, Zero, Zero, One, One, Zero, Zero, One, One, One, Zero, One, One, One, Zero] -- Exemple de chaîne de bits
    putStrLn $ "Bit string: " ++ show bits
    case decode exampleTree bits of
        Just symbols -> putStrLn $ "Decoded symbols: " ++ show symbols
        Nothing      -> putStrLn "Decoding failed"

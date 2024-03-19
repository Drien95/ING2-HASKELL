import Statistic.EncodingTree

tree :: EncodingTree Char
tree =
  EncodingNode 5
    (EncodingNode 4 (EncodingLeaf 2 'A') (EncodingLeaf 2 'B'))
    (EncodingLeaf 1 'C')

-- Tester la fonction totalLength
main :: IO ()
main = do
  let averageLength = meanLength tree
      total = totalLength tree
  putStrLn $ "La longueur moyenne du code est : " ++ show averageLength
  putStrLn $ "Le nombre total de symboles dans l'arbre est : " ++ show total

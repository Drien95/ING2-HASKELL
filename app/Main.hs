import Statistic.EncodingTree

-- Arbre binaire simple
tree :: EncodingTree Char
tree = EncodingNode 5
          (EncodingNode 3
              (EncodingLeaf 1 'a')
              (EncodingLeaf 2 'b'))
          (EncodingLeaf 2 'c')


-- Fonction pour afficher l'arbre
printTree :: Show a => EncodingTree a -> String
printTree (EncodingLeaf cnt symbol) = "Feuille - Compteur : " ++ show cnt ++ ", Symbole : " ++ show symbol ++ "\n"
printTree (EncodingNode cnt left right) = "Noeud - Compteur : " ++ show cnt ++ "\n" ++
                                           "Gauche : " ++ printTree left ++
                                           "Droit  : " ++ printTree right


main :: IO ()
main = putStrLn (printTree tree)

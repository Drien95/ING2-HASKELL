**Projet Méthodes de compression sans perte - Haskell**

**1. Objectif du projet**

Implémenter certaines méthodes de compression sans perte :
- LZ Methods
    - LZ78
    - LZW
- Statistic Methods
    - Huffman
    - Shannon-Fano


**2. Consignes générales**

Projet effectué par groupes de 4 à 5 étudiants

Date limite de rendu (sur CoursCY) : 31 mars 2024, 23h59

Soutenance : semaine du 1er au 5 avril 2024

**3. Consignes spécifiques**

Modification du code existant
*   Modification **INTERDITE** :
    - de la signature des modules
    - de la signature des valeurs et fonctions à implémenter
    - des (quelques) éléments déjà implémentés
- Ajout **INTERDIT** de paquetages supplémentaires
- Ajout autorisé de nouveaux éléments intermédiaires pour
- implémenter les éléments demandés

**4. Nature du rendu**

Fourni : code à completer

À faire : implémenter les éléments manquants dans chaque module

Rendu : archive contenant le répertoire src et rapport court



## Installation

Installer stack avec ghcup ainsi que toutes les autres dépendances : https://www.haskell.org/ghcup/

Puis pull le projet
## Deployment

Pour _exécuter le script_, tapez les commande suivante :

`stack build` : pour installer les dépendances et compiler le projet

`stack exec [path_to_./compress-exe]` : pour executer le programme

`stack test` : pour les tests

Si stack ne marche pas :

`ghc -o exec_name -isrc -iapp app/Main.hs`






## Authors

- [@Drien95](https://github.com/Drien95)
- [@Romcast](https://github.com/Romcast)
- [@Anatpqs](https://github.com/Anatpqs)
- [@LogeshD2](https://github.com/LogeshD2)
- [@WaGit24](https://github.com/WaGit24)


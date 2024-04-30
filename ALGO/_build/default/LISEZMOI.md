# Projet d'Interpolation par Fonctions de Base Radiale (RBF)

## Informations Générales
- **Auteurs** : KHOMSI ELHASSOUNI Abdelmajid, RIAK Ilyas
- **Encadrant** : R. Raffin
- **Année universitaire** : 2023 - 2024
- **Institution** : UFR Sciences et Techniques de Dijon, Master en Informatique, 1re année

## Introduction
Ce projet explore l'application des fonctions de base radiale (RBF) à l'interpolation de données dans le plan. Nous évaluons la capacité des RBF à modéliser des formes géométriques à partir de données dispersées et les comparons avec des méthodes d'interpolation traditionnelles pour leur précision et efficacité.

## Objectifs
1. **Modélisation** : Utiliser les RBF pour reconstruire des formes géométriques complexes à partir de données dispersées.
2. **Comparaison** : Évaluer les performances des RBF par rapport aux méthodes traditionnelles telles que les polynômes et les splines.

## Contenu du Dépôt
- `AnalyseRBF.ml` : Implémentation de l'interpolation par RBF.
- `AnalyseMoindresCarres.ml` : Implémentation de la méthode des moindres carrés.
- `Point.ml` et `Util.ml` : Utilitaires pour la gestion des points et opérations diverses.
- `Polynome.ml` : Manipulations des polynômes utilisés dans les interpolations.
- `main.ml` : Programme principal orchestrant les différentes analyses.

## Méthodologie
### Interpolation par Moindres Carrés
Nous avons utilisé la méthode des moindres carrés pour ajuster une surface définie par une fonction implicite aux données fournies. Cette approche implique la construction d'une matrice de conception basée sur les points de données et la résolution d'un système linéaire pour minimiser l'erreur quadratique.

### Interpolation par Fonctions de Base Radiale (RBF)
Les fonctions de base radiale offrent une flexibilité supérieure pour s'adapter aux configurations complexes de données. Nous avons exploré différentes fonctions de base, comme la gaussienne et la multiquadrique, pour leur capacité à capturer les variations locales des données.

## Résultats
Nous présentons une analyse comparative des résultats obtenus par les deux méthodes, mettant en lumière les avantages et limites de chacune. Nos tests montrent que les RBF peuvent offrir une précision et une flexibilité supérieures sous certaines conditions.

## Comment Utiliser
Pour exécuter le projet, compilez et exécutez `main.ml` par le makefile `make -f makefile.mak` qui orchestre l'ensemble du processus d'interpolation et affiche les résultats. Assurez-vous d'avoir toutes les dépendances nécessaires installées.

# Donneés, codes et macros du Mémoire de Robin Schreiber.
## Etude des barrirères de reproduction intraspecfique chez une espèce de tomate sauvage *Solanum Chilense*

Biologie des organismes et ecologie - UCLouvain - UNamur

Promoteur : Muriel Quinet - Encadrant : Pauline Moreels

Earth Life Institut A - Groupe de recherche en physiologie vegetale 

---
## Table des matières

- [Données Excel](#Données_Excel)
- [Macro Image J](#Macro_image_J)
- [Données_CSV_/_rda](#Données_CSV_/_rda)
- [Code R](#Code_R)
  
---
## Données_Excel

Les données brutes ont été encodé sur Microsoft Excel. Chaque fichiers contient une feuille README.

- **Morphologie_florale.xlsx** : données de morphologie florale des différents individus de *S.chilense*. Ce fichier contient la feuille R_Exertion.
- **Croisement_Fruit_Graine.xlsx** : données des croisements (nombre de croisements réalisés, nombre de fruit, nombre de graines.) entres différents individus de *S.chilense*. Ce fichier contient les feuilles R_Croisements et R_Graine.
- **Croisement_TP.xlsx** : données des six croisements individus de *S.chilense*, avec les nombre de pollen et le nombre de tubes polliniques dans les différents parties du style. Ce fichier contient la feuille R_Tube_polli.
- **Boutures.xlsx** : données d'analyse de développement racinaire de différents individus de *S.chilense* dans différentes solutions de boutruages et données de condutances de solutions. Ce fichier contient les feuilles R_Boutures_Racine et R_Boutures_Conductance
- **S_allele.xlsx** : données des S-allèles et donnée théoriques sur le pourcentage de tubes polliniques atteignant l'ovaire, et taux de mise à fruit, en fonction des situations des S-allèles. Ce fichier contient les feuilles R_S_allele, R_S_allele_the
---
## Macro_Image_J

- **Pistil_Stamen_Length.ijm** : Macro Image J (Fiji) utilisée pour calculer la longuer des pitsils et étamines.

---
## Données_CSV_/_rda

Les données utilisé sur R pour ce mémoire sont des fichiers .csv (sep ";"). Ce sont les feuilles ***R_*** des fichier .xlsx. :
- **R_Exertion.csv** : données de morphologie florale des différents individus de *S.chilense*.
- **R_Croisements.csv** : données des croisements (nombre de croisements réalisés, nombre de fruit, etc.) entres différents individus de *S.chilense*.
- **R_Graine.csv** : données des croisements (nombre de graines par fruit, nombre de graine totale, etc.) entres différents individus de *S.chilense*.
- **R_Tube_polli.csv** : données des 6 croisements, entre individus de *S. chilense* avec le taux de germinations des grains des pollen et la croissance des tubes polliniques dans les différents tiers du style.
- **R_qPCR.csv** : données d'expression relative des gènes calculé par qPCR des différents individus de *S.chilense*.
- **R_Boutures_Racine.csv** : données d'analyse du développement racinaire sur différentes individus de *S.chilense* dans différentes solutions de boutruages.
- **R_Boutures_Conductance.csv** : données de conductance des différentes solutions de bouturages.
- **R_S_allele** : données de présence / absencs des S-allèles des invdividus de *S. chilense*
- **R_S_allele_theory** : données théorique de la compatibtilité au niveau de la croissances des tubes polliniques et du taux de mise à fruit
- **R_Ind_S** : données des S-allèles des individus de *S. chilense*
- **R_Data_all.rda** : compilations des données récoltés, pour l'analysé combinée (voir ACP Discussion - Objectif 1)

----
## Code_R

L'ensemble du code R utilisé pour ce mémoire est rassemblé dans le fichier : **Code_memoire.R**



# Projet de théorie des graphes
# Sources

PREREQUIS à l'utilisation de make :
Dans Makefile, renseigner le chemin vers la librairie
ocamlgraph dans "OCAMLGRAPH_LIB".

Pour compiler le projet :
make

Pour lancer une séquence de test (exécute main_test.ml) :
(Le dag sur lequel les tests sont effectués est (à définir) dans main_test.ml)
make test

Pour lancer l'interpréteur ocaml avec les sources chargées :
make debug

Pour nettoyer les binaires générés :
make clean

Pour nettoyer les binaires générés et les exécutables :
make mrproper

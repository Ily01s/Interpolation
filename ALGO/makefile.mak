# Makefile pour un projet OCaml utilisant Dune

.PHONY: all build clean exec

# Cible par défaut qui va tout construire et exécuter le programme
all: build exec

# Configuration de l'environnement OPAM
setup:
	@eval $$(opam env)

# Compilation du projet
build:
	@dune build

# Exécution du programme
exec:
	@dune exec ./_build/default/main.exe

demo1:
	@dune exec ./_build/default/main.exe -- demo1

demo2:
	@dune exec ./_build/default/main.exe -- demo2

# Nettoyage des fichiers générés
clean:
	@dune clean

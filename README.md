<p align="center">
  <img src="media/ENS_logo.png" alt="Logo ENS" height=80">
  <h2 align="center">ENS Paris-Saclay · DER Informatique</h2>
  <h4 align="center">Projet GOCass</h3>
</p>

---
## À propos
Projet réalisé dans le cadre du cours de Programmation 1 à l'ENS Paris-Saclay. Le sujet détaillé
est disponible dans le dossier `doc`. Le but est de construire un compilateur pour le langage
**Petit Go**. Le parser et le lexer sont déjà fournis, et se basent sur `menhir`.

On distingue deux phases dans le projet:
- le typage d'un arbre de syntaxe abstraite (AST) en un arbre de la syntaxe abstraite
  typé (voir les fichiers `typing.ml`, `ast.mli`, `tast.mli`);
- la production de code x86-64 à partir de cet arbre.

### Structure (sans fichier compilé)

```
.
├── doc
│   ├── slide1.pdf
│   └── sujet.pdf
├── rapport.pdf
├── README.md
├── src
│   ├── ast.mli       # type des AST
│   ├── compile.ml    # création du code assembleur
│   ├── file.ml       # interaction avec les fichiers
│   ├── lexer.mll     # fichier de génération du lexer
│   ├── lib.ml        # fonctions utiles
│   ├── main.ml       # routine principale
│   ├── Makefile
│   ├── parser.mly    # fichier de génération du parser
│   ├── pretty.ml     # affichage des arbres et export en DOT
│   ├── rewrite.ml    # règles de réécriture
│   ├── tast.mli      # type des TAST
│   ├── typing.ml     # typage
│   ├── x86_64.ml     # bibliothèque pour construire du code X86_64
│   └── x86_64.mli
└── tests
    ├── bad
    ├── correctness.sh
    └── good
```

## Installation et dépendances
La compilation utilise `dune` (paquet `ocaml-dune` sur Debian), ainsi que `menhir` (`menhir` sur Debian).
La création de l'exécutable se fait, depuis le dossier `src`, par:
```
make
```

## Utilisation
```
usage: ./pgoc [options] file.go"

--debug       # mode debug: message du compilateur, rendu des arbres
--no-pretty   # affichage des arbres compatibles avec la sortie terminal
--parse-only  # arrête l'exécution après le parsing
--type-only   # arrête l'exécution après le typage
```

## Dépôt
Ce dépôt ne sera certainement pas mis à jour par la suite. Plusieurs détails techniques sont présentés dans le rapport `rapport.pdf`.

## Contact
Lucas Tabary-Maujean, 2022, [e-mail](mailto:l.ta-ma@pm.me)

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
- le typage d'un arbre de syntaxe abstraite (AST) en un arbre de syntaxe abstraite
  typé (voir les fichiers `typing.ml`, `ast.mli`, `tast.mli`);
- la production de code x86-64 à partir de cet arbre.


### Notes de projet, variations par rapport au sujet
- Aucune sémantique n'est donnée pour les procédures. De plus, la sémantique donnée ne permet
  pas de considérer les expressions comme des instructions. J'ai donc étendu ces points.
- La sémantique de _ *(wildcard)* a été étendue pour mieux correspondre à celle du langage Go complet.
  Plus précisément, _ ne peut pas être utilisé comme r-value, peut être associé à autant de valeurs que possible,
  n'a pas besoin d'être déclaré par une structure `var _ = ...`;
- Le fichier `pretty.ml` est celui de v-lafeychine. Il fournit des fichiers DOT qui peuvent ensuite être convertis
  en image vectorielle, par exemple.

### Tests
Les tests s'effectuent en exécutant le script `correctness.sh` du dossier `tests`. Il peut également être appelé
depuis le dossier `src` par `make test`. Les fichiers du dossier `tests/good` (resp. `tests/bad`) doivent être
acceptés (resp. refusés) par le compilateur. Dans le cas contraire, le script affiche la sortie du compilateur.

### Structure (sans fichier compilé)

```
.
├── doc
│   ├── slide1.pdf
│   └── sujet.pdf
├── README.md
├── src
│   ├── ast.mli       # type des AST
│   ├── compile.ml
│   ├── file.ml       # interaction avec les fichiers
│   ├── lexer.mll     # fichier de génération du lexer
│   ├── lib.ml        # fonctions utiles
│   ├── main.ml       # routine principale
│   ├── Makefile
│   ├── parser.mly    # fichier de génération du parser
│   ├── pretty.ml     # affichage des arbres et export en DOT
│   ├── rewrite.ml
│   ├── tast.mli      # type des TAST
│   ├── typing.ml     # typage
│   ├── x86_64.ml
│   └── x86_64.mli
└── tests
    ├── bad
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

## Contact
Lucas Tabary-Maujean, 2021, [e-mail](mailto:l.ta-ma@pm.me)

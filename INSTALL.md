2025-05-06 by Simon Dima
I managed to get things somewhat working by
- removing/hiding an existing `coq` opam switch which was causing builds to fail (including `topkg` and `uutf`)
- making a new branch and removing `.ml` and `.mli` files from the `.gitignore` in `src/extraction/`
- running `opam install . --deps-only`
- running `make` to build the `.ml` and `.mli` files from the Coq sources in `theories/`
- running `opam install .`
- running `make -f CoqMakefile install` to make the LambdaBox package accessible to Coq.
Files are put in `~/opam/switch-name/lib/coq/user-contrib/LambdaBox/` and are accessible within Coq by, for example, `From LambdaBox Require Import EvalBox`.

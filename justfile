default: watch

build:
    dune build

watch:
    dune build @ocaml-index -w

run:
    dune exec menhir-incremental-parser

default: build

build:
    dune build @ocaml-index -w

run:
    dune exec menhir-incremental-parser

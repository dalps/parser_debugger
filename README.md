# `Parser_debugger`

is an interactive debugger for OCaml parsers produced by [Menhir](https://fpottier.gitlabpages.inria.fr/menhir/). It has a nice REPL that lets you step through the shift / reduce transitions the LR(1) automaton takes on while reading an input.

Heavily inspired by the Raku package [Grammar::Debugger](https://raku.land/zef:raku-community-modules/Grammar::Debugger).

## Demo

Given the following library structure for a parser module called `Tiny_parser`:

```
lib/
├── dune
├── main.ml
├── tiny_parser.mly
└── tiny_lexer.mll
```

Your parser must be built with the `--table`, `--inspection`, `--cmly` options. Add these flags in the `dune` file, inside the Menhir stanza:

```
(menhir
 (modules tiny_parser)
 (flags --table --inspection --cmly))

(ocamllex tiny_lexer)
```

Then getting a debugger for your parser is as simple as calling the functor `Parser_debugger.Make` with three things:

```ocaml
(* main.ml *)
module TinyDebugger =
  Parser_debugger.Make
    (struct
      type semantic_value = unit

      let string_of_semval () = "()"
      let mly_path = "lib/tiny_parser.mly"
    end)
    (Tiny_parser)
    (Tiny_lexer)
```

1. A [brief header module](lib/parser_debugger.ml#4) containing some **metadata** about (i) the type of values the parser produces and (ii) their pretty printing, (iii) its location relative to the project root;
1. The **parser** module;
1. The **lexer** module;

That's it! Just call your debugger's `run` method in your main function and have fun:

```ocaml
let _ = TinyDebugger.run ()
```

Check out the other [demos](/demos/).

## Breakpoints

You can decorate your grammar with breakpoints (or set them directly in the REPL) for the following places:

### On a token

after the token's name, or after the alias if present:

```ocaml.menhir
%token <int> INT [@break]
```

The debugger will pause when it consumes or is about to shift token `INT`.

### On a rule
immediately after its name:

```ocaml.menhir
expr [@break rule]:
| ...
```

The debugger will pause when `expr` is about to get reduced (through any of its productions).


### On a production

after the semantic action:

```ocaml.menhir
expr:
| ...
| e1 = expr DIV e2 = expr { e1 / e2 } [@break]
| ...
```

The debugger will pause when `expr -> expr DIV expr` is about to get reduced.

## Installation

> [!IMPORTANT]
> **tl;dr** The debugger is based on an unreleased version of Menhir, which additionally needs to be patched.

The debugger is based on version `20260112` Menhir which introduces all the great API changes that make the debugger possible. Alas, it had to **patch** it to insert a type constraint in the generated parser interfaces that should be there but really isn't.

So, if you can't wait to try the debugger and are up for a bit of hacking, follow along.

Grab Menhir's source code:
```sh
git clone https://gitlab.inria.fr/fpottier/menhir.git/
cd menhir
```

Open [`front/Interface.ml`](https://gitlab.inria.fr/fpottier/menhir/-/blob/release-branch-20260112/front/Interface.ml?ref_type=heads#L261) and replace the `tables` generator with this definition:

```ocaml
let tables () =
  [
    IIComment "The parse tables.";
    IIComment "Warning: this submodule is undocumented. In the future,\n   \
               its type could change, or it could disappear altogether.";
    IIModule (
      "Tables",   (* must agree with [TableBackend] *)
      with_types WKNonDestructive "MenhirLib.TableFormat.TABLES"
      [
        [], "token", ttoken
      ]
    );
    IIComment "hack hack hack";
  ]
```

The type constraint identifies the `token` type of the Monolithic API with the type `Tables.token` type of the [Inspection API](https://cambium.inria.fr/~fpottier/menhir/manual.html#sec64). That's it. This lets the debugger use the `Tables.token2terminal` function provided by the latter API to do an essential conversion.

Time to install the patched Menhir. This might fail due to other packages depending on an older version of Menhir on your opam switch, so work opam problems out:
```sh
dune build @install
```

Finally, rebuild your parser with the patched Menhir. Assuming your parser is called `lib/parser.mly`, a successful patch will have hardcoded the type constraint inside `_build/default/lib/parser.mli`. Look for the following lines:

```ocaml
module Tables : MenhirLib.TableFormat.TABLES
  with type token = token

(* hack hack hack *)
```

The typechecker should now be happy when you apply `Parser_debugger.Make`!

## LICENSE

GPL-v2.0 only
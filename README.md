# Grammar Debugger

`Grammar_debugger` is an interactive debugger for Menhir parsers, with a REPL interface that lets you set *breakpoints* in your grammar and step through the shift / reduce transitions of the generated LR(1) automaton on a given input.

Heavily inspired by Raku's [Grammar::Debugger](https://raku.land/zef:raku-community-modules/Grammar::Debugger).

## Demo

Your parser needs to be built with the `--table`, `--inspection`, `--cmly` flags. Add these to the `dune` file where your parser is declared:

```
(menhir
 (modules tiny_parser)
 (flags --table --inspection --cmly))

(ocamllex tiny_lexer)
```

Then getting a debugger for your parser is as simple as calling the functor `Grammar_debugger.Make`:

```ocaml
module TinyDebugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = unit

      let string_of_semval () = "()"
      let path = "lib/tiny_parser.mly"
    end)
    (Tiny_parser)
    (Tiny_lexer)

let _ = TinyDebugger.run ()
```

We provided:
1. A module containing some [metadata](lib/grammar_debugger.ml#4)
2. The parser module
3. The lexer module

Also check out the other [examples](/examples/).

> [!IMPORTANT]
> **tl;dr** The debugger is based on an unreleased version of Menhir, which additionally needs to be patched.

## Breakpoints

You can decorate your grammar with breakpoints (or set them directly in the command line) for the following places:

### On a token

after the token's name, or the alias if present:

```
%token <int> INT [@break]
```

The debugger will pause when it consumes or is about to shift token `INT`.

### On a rule
immediately after its name:

```
expr [@break rule]:
| ...
```

The debugger will pause when `expr` is about to get reduced (through any of its productions).


### On a production

after the semantic action:

```
expr:
| ...
| e1 = expr DIV e2 = expr { e1 / e2 } [@break]
| ...
```

The debugger will pause when `expr -> expr DIV expr` is about to get reduced.

## Installation

The debugger is based on version `20260112` Menhir (**not released yet!**). Despite the great API additions it comes with that make the debugger possible, I found it needed to be **patched** to insert a type constraint in the generated parser interfaces that should be there but really isn't.

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

In short, this lets the debugger make use of the `token2terminal` function of the `Tables` backend of the [Inspection API](https://cambium.inria.fr/~fpottier/menhir/manual.html#sec64).

Now install the patched Menhir. This might fail due to many packages depending on Menhir and on your switch, so work out any opam problems:
```sh
dune build @install
```

Finally, rebuild your parser with the patched Menhir. Assuming your parser is called `lib/parser.mly`, a successful patch will have hardcoded the type constraint inside `_build/default/lib/parser.mli`. Look for the following lines:

```ocaml
module Tables : MenhirLib.TableFormat.TABLES
  with type token = token

(* hack hack hack *)
```

The typechecker should now be happy when you apply `Grammar_debugger.Make`!

## LICENSE

GPL-v2.0 only
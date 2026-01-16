rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Grammar6.EOL }
| "a" { Grammar6.Ta }
| "b" { Grammar6.Tb }
| "c" { Grammar6.Tc }
| "." { Grammar6.Tc }
| _ { failwith "mini_lexer: unrecognized token!" }

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "a" { Grammar6.Ta }
| "b" { Grammar6.Tb }
| "c" { Grammar6.Tc }
| "." { Grammar6.Tc }
| eof { Grammar6.EOF }
| _ { failwith "mini_lexer: unrecognized token!" }

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "a" { Tiny.Ta }
| "b" { Tiny.Tb }
| "c" { Tiny.Tc }
| "." { Tiny.Tc }
| eof { Tiny.EOF }
| _ { failwith "mini_lexer: unrecognized token!" }

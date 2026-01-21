rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| "a" { Tiny_parser.Ta }
| "b" { Tiny_parser.Tb }
| "c" { Tiny_parser.Tc }
| "." { Tiny_parser.Tc }
| eof { Tiny_parser.EOF }
| _ { failwith "mini_lexer: unrecognized token!" }

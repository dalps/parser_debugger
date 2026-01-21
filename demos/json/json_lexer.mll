{ (* -*- tuareg -*- *)
  open Json

  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let digit = ['0'-'9']

rule token = parse
  (** Layout *)
  | "main:" { token lexbuf }
  | newline { next_line_and token lexbuf }
  | blank+  { token lexbuf               }
  (** With payload *)
  | digit+ as i                             { INT (int_of_string i)     }
  | (digit+ "." digit+) as n                { FLOAT (float_of_string n) }
  | '"' (['a'-'z''A'-'Z''0'-'9''-''_']+ as id) '"'    { ID id }
  | '"' [^'"']* '"' as s                            { STRING s }
  (* Atomic lexemes *)
  | "true"        { TRUE        }
  | "false"       { FALSE       }
  | "null"        { NULL        }
  | "{"           { LEFT_BRACE  }
  | "}"           { RIGHT_BRACE }
  | "["           { LEFT_BRACK  }
  | "]"           { RIGHT_BRACK }
  | ":"           { COLON       }
  | ","           { COMMA       }
  | eof           { EOF         }
  (* Error *)
  | _   { assert false }

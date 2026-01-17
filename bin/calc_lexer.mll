{
  open Calc

  exception Error of string
}


(* This rule analyzes a single line and turns it into a stream of
   tokens. *)
rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf; token lexbuf }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
| eof
    { EOF }

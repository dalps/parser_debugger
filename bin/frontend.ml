module I = Grammar6.MenhirInterpreter

let filename = "_build/default/bin/grammar6.cmly"

module CMLY = MenhirSdk.Cmly_read.Read (struct
  let filename = filename
end)

(* module C = MenhirSdk.Cmly_read.Lift (struct
  let grammar = MenhirSdk.Cmly_read.read_channel (open_in filename)
end) *)

let pr = Printf.printf
let spr = Printf.sprintf

let show_lexbuf (lexbuf : Lexing.lexbuf) =
  let open Lexing in
  pr
    {|lexbuf stats:
  length      : %-8d
  eof reached : %-8b
  contents    : %-8s
  |}
    lexbuf.lex_buffer_len lexbuf.lex_eof_reached
    (Bytes.to_string lexbuf.lex_buffer
    |> String.map (function '\n' -> '\\' | c -> c))

let formatter = Format.formatter_of_out_channel stdout

let string_of_token : Grammar6.token -> string = function
  | Tc -> "Tc"
  | Tb -> "Tb"
  | Ta -> "Ta"
  | EOF -> "EOF"

let string_of_pos (pos : Lexing.position) =
  spr "%d,%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let rec loop ~(lexbuf : Lexing.lexbuf) (checkpoint : unit I.checkpoint) =
  pr "------\n";
  match checkpoint with
  | I.InputNeeded env ->
      pr "Current state: %d\n" (I.current_state_number env);
      print_endline "Enter token (Ctlr+D to end session):";
      let token = Mini_lexer.token lexbuf in
      let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      pr "Recognized token %s[%s-%s]\n" (string_of_token token)
        (string_of_pos startp) (string_of_pos endp);
      I.offer checkpoint (token, startp, endp) |> loop ~lexbuf
  | I.Shifting (_, _, _) ->
      print_endline "Shifting";
      I.resume checkpoint |> loop ~lexbuf
  | I.AboutToReduce (_, prod) ->
      let prod = I.production_index prod |> CMLY.Production.of_int in
      let prod_name = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
      pr "Reducing %s\n" prod_name;
      CMLY.Print.production formatter prod;
      I.resume checkpoint |> loop ~lexbuf
  | I.HandlingError _ -> prerr_endline "syntax error"
  | I.Accepted _ -> print_endline "Accepted."
  | I.Rejected -> failwith "Unreachable."

let () =
  let lexbuf = Lexing.from_channel stdin in
  loop ~lexbuf (Grammar6.Incremental.main Lexing.dummy_pos)

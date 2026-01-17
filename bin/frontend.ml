module G = Calc
module L = Calc_lexer
module I = G.MenhirInterpreter

let filename = "_build/default/bin/calc.cmly"

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
let string_of_token : CMLY.terminal -> string = CMLY.Terminal.name

let string_of_pos (pos : Lexing.position) =
  spr "%d,%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let string_of_item (prod : I.production) (bullet : int) : string * string list =
  let prod = prod |> I.production_index |> CMLY.Production.of_int in
  let lhs = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
  let rhs =
    CMLY.Production.rhs prod |> Array.to_list
    |> List.mapi (fun i (sym, _s, _) -> CMLY.Symbol.name sym)
  in
  (lhs, List.take bullet rhs @ [ "." ] @ List.drop bullet rhs)

let rec loop ~(lexbuf : Lexing.lexbuf) (checkpoint : _ I.checkpoint) =
  pr "###\n";
  match checkpoint with
  | I.InputNeeded env ->
      pr "Current state: %d\n" (I.current_state_number env);
      print_endline "Enter tokens (Ctlr+D to end session):";
      let token = L.token lexbuf in
      let concrete = Lexing.lexeme lexbuf in
      let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      pr "Recognized token %s[%s-%s]\n" concrete (string_of_pos startp)
        (string_of_pos endp);
      I.offer checkpoint (token, startp, endp) |> loop ~lexbuf
  | I.Shifting (_prev, _next, _about_to_accept) ->
      print_endline "Shifting";
      let prev_top, next_top = (I.top _prev, I.top _next) in
      let unwrap_elem (elem : I.element option) =
        match elem with
        | None -> ()
        | Some (I.Element (state, _, startp, endp)) ->
            I.items state
            |> List.iter (fun (prod, j) ->
                let lhs, rhs = string_of_item prod j in
                pr "(bullet is at %d)\n" j;
                pr "%s -> %s\n" lhs (String.concat " " rhs))
      in
      pr "* Previous env:\n";
      unwrap_elem prev_top;
      pr "* Current env:\n";
      unwrap_elem next_top;
      (* TODO: print out the possible follower tokens *)
      I.resume checkpoint |> loop ~lexbuf
  | I.AboutToReduce (_env, prod) ->
      let prod = I.production_index prod |> CMLY.Production.of_int in
      let prod_name = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
      pr "Reducing rule %s\n" prod_name;
      CMLY.Print.production formatter prod;
      I.resume checkpoint |> loop ~lexbuf
  | I.HandlingError _ -> prerr_endline "Syntax error!"
  (* Explain what went wrong please *)
  | I.Accepted _ -> print_endline "Accepted."
  | I.Rejected -> failwith "Unreachable."

let () =
  let lexbuf = Lexing.from_channel stdin in
  let parser_loop = loop in
  let rec menu_loop () =
    pr "#################\n# New Parser Session (%s)\n" CMLY.Grammar.basename;
    parser_loop ~lexbuf (G.Incremental.main Lexing.dummy_pos);
    menu_loop ()
  in
  menu_loop ()

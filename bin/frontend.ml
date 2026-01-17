module G = Calc
module L = Calc_lexer
module I = G.MenhirInterpreter
module T = ANSITerminal
open T

let filename = "_build/default/bin/calc.cmly"

module CMLY = MenhirSdk.Cmly_read.Read (struct
  let filename = filename
end)

(* module C = MenhirSdk.Cmly_read.Lift (struct
  let grammar = MenhirSdk.Cmly_read.read_channel (open_in filename)
end) *)

let log ?(style = []) =
  Printf.ksprintf (fun s -> T.print_string style (s ^ "\n"))

let pr ?(style = []) = Printf.ksprintf (T.print_string style)
let spr = Printf.sprintf

let show_lexbuf (lexbuf : Lexing.lexbuf) =
  let open Lexing in
  log
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

let string_of_production (prod : I.production) : string =
  let prod = prod |> I.production_index |> CMLY.Production.of_int in
  let lhs = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
  let rhs =
    CMLY.Production.rhs prod |> Array.to_list
    |> List.mapi (fun i (sym, _s, _) -> CMLY.Symbol.name sym)
  in
  spr "%s -> %s" lhs (String.concat " " rhs)

let rec loop ~(lexbuf : Lexing.lexbuf) (checkpoint : _ I.checkpoint) =
  log ~style:[ blue ] "---";
  match checkpoint with
  | I.InputNeeded env -> (
      log "Current state: %d" (I.current_state_number env);
      log "Enter tokens (Ctlr+D to end session):";
      flush_all ();
      try
        let token = L.token lexbuf in
        let concrete = Lexing.lexeme lexbuf in
        let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in

        pr ~style:[ Background Green ] "MATCH";
        log ~style:[ green ] " \"%s\" [%s-%s]" concrete (string_of_pos startp)
          (string_of_pos endp);
        I.offer checkpoint (token, startp, endp) |> loop ~lexbuf
      with _ ->
        pr ~style:[ Background Red ] "FAIL";
        log ~style:[ red ] " illegal token")
  | I.Shifting (_prev, _next, _about_to_accept) ->
      log "Shifting";
      let prev_top, next_top = (I.top _prev, I.top _next) in
      let unwrap_elem (elem : I.element option) =
        match elem with
        | None -> ()
        | Some (I.Element (state, _, startp, endp)) ->
            I.items state
            |> List.iter (fun (prod, j) ->
                let lhs, rhs = string_of_item prod j in
                log "(bullet is at %d)" j;
                log "%s -> %s" lhs (String.concat " " rhs))
      in
      log "* Previous env:";
      unwrap_elem prev_top;
      log "* Current env:";
      unwrap_elem next_top;
      (* TODO: print out the possible follower tokens *)
      I.resume checkpoint |> loop ~lexbuf
  | I.AboutToReduce (_env, prod) ->
      let prod_name =
        prod |> I.production_index |> CMLY.Production.of_int
        |> CMLY.Production.lhs |> CMLY.Nonterminal.name
      in
      pr "Reducing rule ";
      log ~style:[ Bold ] "%s" prod_name;
      log "%s" (string_of_production prod);
      I.resume checkpoint |> loop ~lexbuf
  | I.HandlingError _ -> log ~style:[ red ] "Syntax error!"
  (* Explain what went wrong please *)
  | I.Accepted _ -> log ~style:[ green ] "Accepted."
  | I.Rejected -> failwith "Unreachable."

let () =
  let lexbuf = Lexing.from_channel stdin in
  let parser_loop = loop in
  let rec menu_loop () =
    log ~style:[ blue ] "---";
    log ~style:[ Bold; blue ] "New parse (%s)" CMLY.Grammar.basename;
    parser_loop ~lexbuf (G.Incremental.main Lexing.dummy_pos);
    menu_loop ()
  in
  menu_loop ()

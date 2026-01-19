module T = ANSITerminal
module ML = MenhirLib
module MS = MenhirSdk
open T

let ( % ) f g x = g (f x)

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

module Make
  (* (Basic: ML.EngineTypes.MONOLITHIC_ENGINE) *)
  (Basic: sig
    type token
    exception Error
    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> int
  end)
  (Lexer: sig
    exception Error of string
    val token : Lexing.lexbuf -> Basic.token
  end)
  (* (Interpreter : ML.EngineTypes.ENGINE with type token = Basic.token) *)
  (Tables: ML.TableFormat.TABLES with type token = Basic.token)
  (Interpreter : ML.IncrementalEngine.EVERYTHING with type token = Basic.token)
  (Incremental : sig
    val main: Lexing.position -> (int) Interpreter.checkpoint
  end)
  (Cmly_path : sig val filename : string end) =
struct
module B = Basic
module L = Lexer
module I = Interpreter
module CMLY = MenhirSdk.Cmly_read.Read (Cmly_path)

open CMLY

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

let string_of_cmly_prod (prod : CMLY.Production.t) : string =
  let lhs = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
  let rhs =
    CMLY.Production.rhs prod |> Array.to_list
    |> List.mapi (fun i (sym, _s, _) -> CMLY.Symbol.name sym)
  in
  spr "%s -> %s" lhs (String.concat " " rhs)

let string_of_production (prod : I.production) : string =
  prod |> I.production_index |> CMLY.Production.of_int |> string_of_cmly_prod

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
        let terminal = Tables.token2terminal token |> CMLY.Terminal.of_int |> string_of_token in
        let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in

        pr ~style:[ green ] "MATCH ";
        pr ~style:[ Background Green ] "%s" terminal;
        log ~style:[ green ] ": \"%s\" [%s-%s]" concrete (string_of_pos startp)
          (string_of_pos endp);

        I.offer checkpoint (token, startp, endp) |> loop ~lexbuf
      with _ ->
        pr ~style:[ Background Red ] "FAIL";
        log ~style:[ red ] " Unrecognized token.")
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

module type FoldAttrs = sig
  type t

  val fold : (t -> 'a -> 'a) -> 'a -> 'a
  val attributes : t -> Attribute.t list
end

module Breakpoint = struct
  type 'a t = { elem : 'a; pos : Range.t; eq : string option }

  let create elem pos eq = { elem; pos; eq }
end

(** [get_breakpoints (module K)] collects breakpoints of kind [K] of the
    grammar. *)
let get_breakpoints (type k) (module K : FoldAttrs with type t = k) :
    k Breakpoint.t list =
  K.fold
    (fun t acc ->
      match K.attributes t with
      | attr :: _ when Attribute.label attr = "break" ->
          Breakpoint.create t (Attribute.position attr)
            (match Attribute.payload attr with "" -> None | eq -> Some eq)
          :: acc
      | _ -> acc)
    []

let run () =
  let bp_terminals = get_breakpoints (module Terminal) in
  let bp_productions = get_breakpoints (module Production) in
  let bp_rules = get_breakpoints (module Nonterminal) in

  log ~style:[ yellow ] "Terminal breakpoints:";
  bp_terminals
  |> List.iter (fun b -> log "%s" (Terminal.name b.Breakpoint.elem));
  log ~style:[ yellow ] "Production breakpoints:";
  bp_productions
  |> List.iter (fun b -> log "%s" (string_of_cmly_prod b.Breakpoint.elem));
  log ~style:[ yellow ] "Nonterminal breakpoints:";
  bp_rules |> List.iter (fun b -> log "%s" (Nonterminal.name b.Breakpoint.elem));

  let lexbuf = Lexing.from_channel stdin in
  let parser_loop = loop in
  let rec menu_loop () =
    log ~style:[ blue ] "---";
    log ~style:[ Bold; blue ] "New parse (%s)" CMLY.Grammar.basename;
    parser_loop ~lexbuf (Incremental.main Lexing.dummy_pos);
    menu_loop ()
  in
  menu_loop ()
end

module Calc = Make (Calc) (Calc_lexer) (Calc.Tables) (Calc.MenhirInterpreter) (Calc.Incremental) (struct
  let filename = "_build/default/bin/calc.cmly"
end)

let _ = Calc.run ()

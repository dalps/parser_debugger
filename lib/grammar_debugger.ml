open Utils
open ANSITerminal

module type METADATA = sig
  type semantic_value
  (** The type of values produced by the grammar's entry point. *)

  val string_of_semval : semantic_value -> string
  (** Converts a semantic value to a string for pretty printing. *)

  val path : string
  (** The path to the grammar's .mly file relative to the root of the project.
      (e.g. ["lib/parser.mly"]) *)
end

(** [Make (Meta) (Parser) (Lexer)] makes an instance of the debugger with given metadata, [Parser] and [Lexer].
- [Meta] specifies the type of semantic values produced by the parser, how to display them, and where the grammar is located in the dune project.
- [Parser] is the module produced by Menhir with the [--table] and [--inspection] switches.
- [Lexer] is exactly the lexer module.
*)
module Make
    (X : METADATA)
    (Parser : sig
      type token

      module Tables : ML.TableFormat.TABLES with type token = token

      module MenhirInterpreter :
        ML.IncrementalEngine.EVERYTHING with type token = token

      module Incremental : sig
        val main :
          Lexing.position -> X.semantic_value MenhirInterpreter.checkpoint
      end
    end)
    (Lexer : sig
      val token : Lexing.lexbuf -> Parser.token
    end) =
struct
  module B = Parser
  module L = Lexer
  module I = B.MenhirInterpreter
  module Tables = B.Tables

  module CMLY = MenhirSdk.Cmly_read.Read (struct
    let filename =
      let open Filename in
      concat "_build/default" (remove_extension X.path ^ ".cmly")
  end)

  open CMLY

  module type FoldAttrs = sig
    type t

    val fold : (t -> 'a -> 'a) -> 'a -> 'a
    val attributes : t -> Attribute.t list
  end

  module Breakpoint = struct
    type 'a t = { elem : 'a; pos : Range.t; eq : string option }

    let create elem pos eq = { elem; pos; eq }
  end

  type 'a breakpoints = 'a Breakpoint.t list

  type state = {
    bp_terminals : CMLY.terminal breakpoints;
    bp_productions : CMLY.production breakpoints;
    bp_rules : CMLY.nonterminal breakpoints;
    lexbuf : Lexing.lexbuf;
    manual_input : bool;
  }

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

  let string_of_token : CMLY.terminal -> string = CMLY.Terminal.name

  let string_of_pos (pos : Lexing.position) =
    spr "%d,%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

  let bullet_char = "\u{2022}"

  let string_of_item (prod : I.production) (bullet : int) : string * string list
      =
    let prod = prod |> I.production_index |> CMLY.Production.of_int in
    let lhs = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
    let rhs =
      CMLY.Production.rhs prod |> Array.to_list
      |> List.mapi (fun i (sym, _s, _) -> CMLY.Symbol.name sym)
    in
    (lhs, List.take bullet rhs @ [ bullet_char ] @ List.drop bullet rhs)

  let string_of_cmly_prod (prod : CMLY.Production.t) : string =
    let lhs = CMLY.Production.lhs prod |> CMLY.Nonterminal.name in
    let rhs =
      CMLY.Production.rhs prod |> Array.to_list
      |> List.mapi (fun i (sym, _s, _) -> CMLY.Symbol.name sym)
    in
    spr "%s -> %s" lhs (String.concat " " rhs)

  let string_of_production (prod : I.production) : string =
    prod |> I.production_index |> CMLY.Production.of_int |> string_of_cmly_prod

  module Commands = struct
    type t =
      | Run
      | Step
      | RunFail
      | Quit
      | List
      | BP_list
      | BP_rm of string
      | BP_add of string

    let tab = "    "

    let list_breakpoints (state : state) =
      log ~style:[ yellow ] "Token breakpoints:";
      state.bp_terminals
      |> List.iter (fun b ->
          log "    %s%s"
            (Terminal.name b.Breakpoint.elem)
            (Option.fold ~none:"" ~some:(fun s -> spr " = \'%s\'" s) b.eq));
      log ~style:[ yellow ] "Production breakpoints:";
      state.bp_productions
      |> List.iter (fun b ->
          log "    %s" (string_of_cmly_prod b.Breakpoint.elem));
      log ~style:[ yellow ] "Rule breakpoints:";
      state.bp_rules
      |> List.iter (fun b -> log "    %s" (Nonterminal.name b.Breakpoint.elem))

    let help () =
      pr
        {|    r              run (until breakpoint, if any)
    <enter>        single step
    rf             run until a match fails
    r <name>       run until rule or token <name> is reached
    bp add <name>  add a breakpoint on <name>
    bp list        list all active breakpoints
    bp rm <name>   remove breakpoint on <name>
    bp rm          removes all breakpoints
    q              quit
|}

    let rec prompt_command () : t =
      pr "> ";
      match read_line () with
      | "" -> Step
      | "r" -> Run
      | "rf" -> RunFail
      | "bp add" -> BP_add ""
      | "bp rm" -> BP_rm ""
      | "bp list" -> BP_list
      | "q" | "quit" -> exit 0
      | _ ->
          help ();
          prompt_command ()

    let prompt_input () =
      log "The parser stopped for input";
      pr ">";
      read_line ()
  end

  let rec loop (state : state) (checkpoint : _ I.checkpoint) =
    let open Commands in
    let { lexbuf } = state in
    let rec repl () =
      match prompt_command () with
      | Commands.Quit ->
          log "Goodbye.";
          exit 0
      | Commands.BP_list ->
          list_breakpoints state;
          repl ()
      | _ -> ()
    in
    repl ();
    match checkpoint with
    | I.InputNeeded env -> (
        log "Current state: %d" (I.current_state_number env);
        if state.manual_input then log "Enter tokens (Ctlr+D to end session):";
        flush_all ();
        try
          let token = L.token lexbuf in
          let concrete = Lexing.lexeme lexbuf in
          let terminal =
            Tables.token2terminal token
            |> CMLY.Terminal.of_int |> string_of_token
          in
          let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in

          pr ~style:[ green ] "MATCH ";
          pr ~style:[ Background Green ] "%s" terminal;
          log ~style:[ green ] ": \"%s\" [%s-%s]" concrete
            (string_of_pos startp) (string_of_pos endp);

          I.offer checkpoint (token, startp, endp) |> loop state
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
                  log "%s -> %s" lhs (String.concat " " rhs))
        in
        log "* Previous env:";
        unwrap_elem prev_top;
        log "* Current env:";
        unwrap_elem next_top;
        (* TODO: print out the possible follower tokens *)
        I.resume checkpoint |> loop state
    | I.AboutToReduce (_env, prod) ->
        let prod_name =
          prod |> I.production_index |> CMLY.Production.of_int
          |> CMLY.Production.lhs |> CMLY.Nonterminal.name
        in
        pr "Reducing rule ";
        log ~style:[ Bold ] "%s" prod_name;
        log "%s" (string_of_production prod);
        I.resume checkpoint |> loop state
    | I.HandlingError _ -> log ~style:[ red ] "Syntax error!"
    (* Explain what went wrong please *)
    | I.Accepted x ->
        log ~style:[ green ] "Accepted.";
        log ~style:[ green ] "Result: %s" (X.string_of_semval x)
    | I.Rejected -> failwith "unreachable"

  let run ?(input = "") () =
    let bp_terminals = get_breakpoints (module Terminal) in
    let bp_productions = get_breakpoints (module Production) in
    let bp_rules = get_breakpoints (module Nonterminal) in
    let lexbuf, manual_input =
      match input with
      | "" -> (Lexing.from_channel stdin, true)
      | s -> (Lexing.from_string s, false)
    in
    let s0 : state =
      { bp_terminals; bp_productions; bp_rules; lexbuf; manual_input }
    in
    pr ~style:[ blue ] "debug_grammar(";
    pr ~style:[ Bold ] "%s" X.path;
    log ~style:[ blue ] ")\n";
    log ~style:[] "Type '?' or 'help' for a list of commands.";
    loop s0 (B.Incremental.main Lexing.dummy_pos)
end

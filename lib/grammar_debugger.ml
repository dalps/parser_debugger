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

(** [Make (X) (Parser) (Lexer)] creates an instance of the debugger for a
    particular grammar.
    - [X] describes the type of semantic values produced by the start symbol,
      how to display them, and where the grammar is located in your dune
      project.
    - [Parser] is the module produced by Menhir with the [--table] and
      [--inspection] switches set. The entry point must be called [main].
    - [Lexer] is exactly the lexer module. It must have an entry point called
      [token]. *)
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

  open X
  open CMLY

  module type FoldAttrs = sig
    type t

    val fold : (t -> 'a -> 'a) -> 'a -> 'a
    val attributes : t -> Attribute.t list
  end

  module Breakpoint = struct
    type 'a t = { elem : 'a; pos : Range.t; eq : string option }

    let create elem pos eq = { elem; pos; eq }
    let elem t = t.elem
  end

  type 'a breakpoints = 'a Breakpoint.t list

  type state = {
    bp_terminals : CMLY.terminal breakpoints;
    bp_productions : CMLY.production breakpoints;
    bp_rules : CMLY.nonterminal breakpoints;
    lexbuf : Lexing.lexbuf;
    manual_input : bool;
    checkpoint : semantic_value I.checkpoint;
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
      | DumpLexer
      | Peek
      | BP_list
      | BP_rm of string
      | BP_add of string

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
    p              inspect parser stack
    l              inspect lexer buffer
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
      | "l" -> DumpLexer
      | "p" -> Peek
      | "q" | "quit" -> Quit
      | _ ->
          help ();
          prompt_command ()

    let prompt_input () =
      log "The parser stopped for input";
      pr ">";
      read_line ()
  end

  let unwrap_elem (elem : I.element option) =
    match elem with
    | None -> ()
    | Some (I.Element (state, _, startp, endp)) ->
        I.items state
        |> List.iter (fun (prod, j) ->
            let lhs, rhs = string_of_item prod j in
            log "%s -> %s" lhs (String.concat " " rhs))

  let show_top (env : X.semantic_value I.env) = unwrap_elem (I.top env)

  let peek_env (c : X.semantic_value I.checkpoint) :
      X.semantic_value I.env option =
    match c with
    | I.InputNeeded e
    | I.Shifting (e, _, _) (* e is the env before shifting *)
    | I.AboutToReduce (e, _) (* e is the env before reducing *)
    | I.HandlingError e ->
        Some e
    | I.Accepted _ -> None
    | I.Rejected -> None

  (** Presents info about the parser's state after hitting a breakpoint or by
      request of the user. *)
  let dump_info (state : state) =
    ignore
    @@
    let open O in
    let+ env = peek_env state.checkpoint in
    log "The parser is now in state %d." (I.current_state_number env);
    log "The LR(0) items of topmost state of the stack are:";
    show_top env;
    log "At this state, the parser will now %s."
      (match state.checkpoint with
      | I.InputNeeded _ -> "prompt for input"
      | I.Shifting (_, _, _) -> "shift"
      | I.AboutToReduce (_, _) -> "reduce"
      | I.HandlingError _ -> "handle an error"
      | I.Accepted _ -> "accept the input"
      | I.Rejected -> "reject the input")

  let read_token
      ({
         bp_terminals;
         bp_productions;
         bp_rules;
         lexbuf;
         manual_input;
         checkpoint;
       } :
        state) =
    if manual_input then log "Enter tokens (Ctlr+D for EOF):";
    flush_all ();
    try
      let token = L.token lexbuf in
      let concrete = Lexing.lexeme lexbuf in
      let terminal = Tables.token2terminal token |> Terminal.of_int in
      let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in

      pr ~style:[ Background Green ] " MATCH ";
      pr ~style:[ green; Bold ] " %s" (Terminal.name terminal);
      log ~style:[ green ] ": \"%s\" [%s-%s]" concrete (string_of_pos startp)
        (string_of_pos endp);
      Some (token, startp, endp, terminal)
    with _ ->
      pr ~style:[ Background Red ] " ERROR ";
      log ~style:[ red ] " Unrecognized token.";
      None

  let rec run
      ({
         bp_terminals;
         bp_productions;
         bp_rules;
         lexbuf;
         manual_input;
         checkpoint;
       } as state :
        state) : state =
    let open O in
    match checkpoint with
    | I.AboutToReduce (_, p) ->
        let prod = I.production_index p |> Production.of_int in
        let rule = Production.lhs prod in
        let prod_bp =
          List.exists (Breakpoint.elem % Production.equal prod) bp_productions
        in
        let rule_bp =
          List.exists (Breakpoint.elem % Nonterminal.equal rule) bp_rules
        in
        let state' = { state with checkpoint = I.resume checkpoint } in
        if prod_bp then log_info "Paused at production breakpoint.";
        if rule_bp then log_info "Paused at rule breakpoint.";
        if prod_bp || rule_bp then state' else run state'
    | I.InputNeeded _ -> (
        match read_token state with
        | None -> state
        | Some (token, startp, endp, terminal) ->
            let state' =
              {
                state with
                checkpoint = I.offer checkpoint (token, startp, endp);
              }
            in
            if
              List.exists
                (Breakpoint.elem % Terminal.equal terminal)
                bp_terminals
            then (
              log_info "Paused at %s breakpoint." (Terminal.name terminal);
              state')
            else run state')
    | I.Shifting (_, _, _) ->
        run { state with checkpoint = I.resume checkpoint }
    | I.HandlingError _ | I.Accepted _ | I.Rejected -> state

  let step
      ({
         bp_terminals;
         bp_productions;
         bp_rules;
         lexbuf;
         manual_input;
         checkpoint;
       } as state :
        state) : state =
    match checkpoint with
    | I.InputNeeded env -> (
        log "The parser is consuming input from the buffer.";
        match read_token state with
        | None -> state
        | Some (token, startp, endp, _) ->
            { state with checkpoint = I.offer checkpoint (token, startp, endp) }
        )
    | I.Shifting (_, env, _about_to_accept) ->
        log "The parser has shifted a token.";
        show_top env;
        { state with checkpoint = I.resume checkpoint }
    | I.AboutToReduce (_env, prod) ->
        let prod_name =
          prod |> I.production_index |> CMLY.Production.of_int
          |> CMLY.Production.lhs |> CMLY.Nonterminal.name
        in
        pr "The parser has reduced the following production of rule ";
        log ~style:[ Bold ] "%s" prod_name;
        log "%s" (string_of_production prod);
        { state with checkpoint = I.resume checkpoint }
    | I.HandlingError env ->
        log ~style:[ red ] "Syntax error!";
        show_top env;
        state
    (* Explain what went wrong please *)
    | I.Accepted x ->
        log ~style:[ green ] "Accepted.";
        log ~style:[ green ] "Result: %s" (X.string_of_semval x);
        state
    | I.Rejected -> failwith "unreachable"

  let rec loop
      ({ bp_terminals; bp_productions; bp_rules; lexbuf; manual_input } as state :
        state) : unit =
    let open Commands in
    match prompt_command () with
    | DumpLexer ->
        show_lexbuf state.lexbuf;
        loop state
    | Quit ->
        log "Goodbye.";
        exit 0
    | BP_list ->
        list_breakpoints state;
        loop state
    | Peek ->
        dump_info state;
        loop state
    | Step -> step state |> loop
    | Run ->
        let state' = run state in
        dump_info state';
        loop state'
    | _ -> ()

  let run ?(input : [> `File of string | `Text of string ] option) () =
    let bp_terminals = get_breakpoints (module Terminal) in
    let bp_productions = get_breakpoints (module Production) in
    let bp_rules = get_breakpoints (module Nonterminal) in
    let lexbuf, manual_input =
      match input with
      | None -> (Lexing.from_channel stdin, true)
      | Some (`File f) ->
          let ch = open_in f in
          let lexbuf = Lexing.from_string (In_channel.input_all ch) in
          close_in ch;
          (lexbuf, false)
      | Some (`Text s) -> (Lexing.from_string s, false)
    in
    let chk0 = B.Incremental.main Lexing.dummy_pos in
    let s0 : state =
      {
        bp_terminals;
        bp_productions;
        bp_rules;
        lexbuf;
        manual_input;
        checkpoint = chk0;
      }
    in
    pr ~style:[ blue ] "debug_grammar(";
    pr ~style:[ Bold ] "%s" X.path;
    log ~style:[ blue ] ")\n";
    log ~style:[] "Type '?' or 'help' for a list of commands.";
    loop s0
end

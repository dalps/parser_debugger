module I = Grammar6.MenhirInterpreter

let rec loop (checkpoint : unit I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      print_endline "waiting for a token:";
      let lexbuf = read_line () |> Lexing.from_string in
      let token = Mini_lexer.token lexbuf in
      let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      I.offer checkpoint (token, startp, endp) |> loop
  | I.Shifting (_, _, _) ->
      print_endline "shift";
      I.resume checkpoint |> loop
  | I.AboutToReduce (_, prod) ->
      print_endline "reduce";
      I.resume checkpoint |> loop
  | I.HandlingError _ -> prerr_endline "syntax error"
  | I.Accepted _ -> print_endline "accepted."
  | I.Rejected -> failwith "unreachable"

let () =
  loop (Grammar6.Incremental.rule_S Lexing.dummy_pos)

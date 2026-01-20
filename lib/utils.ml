module T = ANSITerminal
module ML = MenhirLib
module MS = MenhirSdk
open T

module O = struct
  include Option

  let ( let+ ) o f = Option.map f o
  let ( let* ) = Option.bind
end

let ( % ) f g x = g (f x)

let log ?(style = []) =
  Printf.ksprintf (fun s -> T.print_string style (s ^ "\n"))

let log_info ?(style = []) = log ~style:(blue :: style)
let pr ?(style = []) = Printf.ksprintf (T.print_string style)
let spr = Printf.sprintf

let show_lexbuf (lexbuf : Lexing.lexbuf) =
  let open Lexing in
  log
    {|lexbuf stats:
  length      : %-8d
  eof reached : %-8b
  contents    : %-8s
  pos         : %-8d
|}
    lexbuf.lex_buffer_len lexbuf.lex_eof_reached
    (Bytes.to_string lexbuf.lex_buffer
    |> String.map (function '\n' -> '\\' | c -> c))
    lexbuf.lex_abs_pos

let formatter = Format.formatter_of_out_channel stdout

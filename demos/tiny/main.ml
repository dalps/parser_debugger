module TinyDebugger =
  Parser_debugger.Make
    (struct
      type semantic_value = unit

      let string_of_semval () = "()"
      let mly_path = "demos/tiny/tiny_parser.mly"
    end)
    (Tiny_parser)
    (Tiny_lexer)

let _ = TinyDebugger.run ()

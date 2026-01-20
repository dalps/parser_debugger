module CalcDebugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = int

      let string_of_semval = string_of_int
      let path = "examples/calc.mly"
    end)
    (Calc)
    (Calc_lexer)

module TinyDebugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = unit

      let string_of_semval () = "()"
      let path = "examples/tiny.mly"
    end)
    (Tiny)
    (Tiny_lexer)

module JsonDebugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = Ast.value

      let string_of_semval = Ast.to_string
      let path = "examples/json.mly"
    end)
    (Json)
    (Json_lexer)

let _ =
  JsonDebugger.run ~input:(`File "examples/test.json") ();
  CalcDebugger.run ~input:(`Text "1 + (42-2) / 2 * (7-0) + (4 - 2 - 1)") ();
  (* This last line will prompt you for tokens. *)
  TinyDebugger.run ()

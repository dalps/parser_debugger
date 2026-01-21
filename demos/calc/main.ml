module CalcDebugger =
  Parser_debugger.Make
    (struct
      type semantic_value = int

      let string_of_semval = string_of_int
      let mly_path = "demos/calc/calc.mly"
    end)
    (Calc)
    (Calc_lexer)

let _ =
  CalcDebugger.run ~input:(`Text "1 + (42-2) / 2 * (7-0) + (4 - 2 - 1)") ()

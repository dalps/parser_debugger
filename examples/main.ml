module CalcDebugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = int

      let string_of_semval = string_of_int
      let path = "examples/calc.mly"
    end)
    (Calc)
    (Calc_lexer)

module Grammar6Debugger =
  Grammar_debugger.Make
    (struct
      type semantic_value = unit

      let string_of_semval () = "()"
      let path = "examples/grammar6.mly"
    end)
    (Grammar6)
    (Mini_lexer)

let _ = Grammar6Debugger.run ()

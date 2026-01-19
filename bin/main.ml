module CalcDebugger = Frontend.Make (struct
  type semantic_value = int
  let string_of_semval = string_of_int
  let path = "bin/calc.mly"
end) (Calc) (Calc_lexer)

module Grammar6Debugger = Frontend.Make (struct
  type semantic_value = unit
  let string_of_semval () = "()"
  let path = "bin/grammar6.mly"
end) (Grammar6) (Mini_lexer)

let _ = Grammar6Debugger.run ()

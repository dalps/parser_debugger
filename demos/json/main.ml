module JsonDebugger =
  Parser_debugger.Make
    (struct
      type semantic_value = Ast.value

      let string_of_semval = Ast.to_string
      let mly_path = "demos/json/json.mly"
    end)
    (Json)
    (Json_lexer)

let _ = JsonDebugger.run ~input:(`File "demos/json/test.json") ()

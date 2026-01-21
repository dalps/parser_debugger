type value =
  [ `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string ]

let spr = Printf.sprintf

let rec to_string (t : value) =
  match t with
  | `Bool true -> "true"
  | `Bool false -> "false"
  | `Null -> "null"
  | `Assoc [] -> "{}"
  | `Assoc xs ->
      spr "{ %s }"
      @@ String.concat ", "
           (List.map (fun (k, v) -> spr "\"%s\": %s" k (to_string v)) xs)
  | `List [] -> "[]"
  | `List xs -> spr "[ %s ]" @@ String.concat "," (List.map to_string xs)
  | `String s -> spr "\"%s\"" s
  | `Float n -> string_of_float n
  | `Int n -> string_of_int n

(*
  Working with turing machines.
*)


(*
  Parse a string containing a two states and a list of rules.
  TODO: Check that rules don't overlap
*)
let parse_program string =
  try
    ProgramParser.main ProgramLexer.token (Lexing.from_string string)
  with
    | Parsing.Parse_error ->
        raise (Failure "Could not parse program")
    | ProgramLexer.Eof ->
        raise (Failure "Empty program definition")


(*
  Parse a string of digits and blanks.
*)
let rec parse_tape string =
  try
    let head = match String.sub string 0 1 with
      | " " -> None
      | c   -> Some (int_of_string c)
    and tail = String.sub string 1 ((String.length string) - 1)
    in
    head :: (parse_tape tail)
  with
    | Invalid_argument _ -> []
    | Failure _          ->
        let e = "Only digits and blanks are allowed on the tape" in
        raise (Failure e)

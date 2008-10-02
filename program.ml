(*
  Implementation for the program datatype.
*)


type state         = string
type configuration = state * Tape.symbol
type action        = state * Tape.symbol * Tape.direction
type rule          = configuration * action

module ConfigurationMap = Map.Make(struct
                            type t = configuration
                            let compare = compare
                          end)

type rules   = action ConfigurationMap.t
type program = rules * state * state

exception Halted
exception Diverged


(*
  Create program from string.
*)
let parse string =
  let rules, initial_state, halting_state =
    try
      ProgramParser.main ProgramLexer.token (Lexing.from_string string)
    with
      | Parsing.Parse_error ->
          raise (Failure "Could not parse program")
      | ProgramLexer.Eof ->
          raise (Failure "Empty program definition")
  and f rules (configuration, action) =
    ConfigurationMap.add configuration action rules
  in
  List.fold_left f ConfigurationMap.empty rules, initial_state, halting_state


(*
  Apply one rule.
*)
let step program configuration =
  let rules, initial_state, halting_state = program
  and state, symbol = configuration
  in
  if state = halting_state then
    raise Halted
  else
    try
      ConfigurationMap.find configuration rules
    with
      | Not_found -> raise Diverged

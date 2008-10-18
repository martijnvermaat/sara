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
  Create program from rule list, initial state and halting state.
*)
let create rules initial_state halting_state =
  rules, initial_state, halting_state


(*
  Create program from string.
*)
let parse string =
  let rules, initial_state, halting_state =
    try
      ProgramParser.program ProgramLexer.token (Lexing.from_string string)
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
  Create list of rules from string.
*)
let parse_rules string =
  let rules =
    try
      ProgramParser.rules ProgramLexer.token (Lexing.from_string string)
    with
      | Parsing.Parse_error ->
          raise (Failure "Could not parse rules")
      | ProgramLexer.Eof ->
          raise (Failure "No rules")
  and f rules (configuration, action) =
    ConfigurationMap.add configuration action rules
  in
  List.fold_left f ConfigurationMap.empty rules


(*
  Pretty print rule to string.
*)
let pretty_print_rule rule =
  let (state, symbol), (state', symbol', direction) = rule in
  state
  ^ " " ^ (Tape.pretty_print_symbol symbol)
  ^ " " ^ state'
  ^ " " ^ (Tape.pretty_print_symbol symbol')
  ^ " " ^ (Tape.pretty_print_direction direction)


(*
  Pretty print program to strings.
*)
let pretty_print program =
  let rules, initial_state, halting_state = program
  and f configuration action rules =
    pretty_print_rule (configuration, action)
    ^ "\n" ^ rules
  in
  (ConfigurationMap.fold f rules ""), initial_state, halting_state


(*
  Program's initial state.
*)
let initial_state program =
  let _, state, _ = program in
  state


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

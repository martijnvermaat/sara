(*
  Implementation of a turing machine.
*)


type state   = string
type rule    = { current_state  : state;
                 current_symbol : Tape.symbol;
                 new_state      : state;
                 new_symbol     : Tape.symbol;
                 direction      : Tape.direction }

module ConfigurationMap = Map.Make(struct
                            type t = state * Tape.symbol
                            let compare = compare
                          end)

type rules = (state * Tape.symbol * Tape.direction) ConfigurationMap.t

type machine = { rules         : rules;
                 state         : state;
                 halting_state : state;
                 tape          : Tape.tape }

exception Halted
exception Diverged


(*
  New turing machine.
*)
let create rules initial_state halting_state symbols =
  let f rules rule =
    ConfigurationMap.add
      (rule.current_state, rule.current_symbol)
      (rule.new_state, rule.new_symbol, rule.direction)
      rules
  in
  { rules         = List.fold_left f ConfigurationMap.empty rules;
    state         = initial_state;
    halting_state = halting_state;
    tape          = Tape.create symbols }


(*
  Apply one rule.
*)
let step machine =
  if machine.state = machine.halting_state then
    raise Halted
  else
    try
      let state, symbol, direction = ConfigurationMap.find
        (machine.state, Tape.read machine.tape)
        machine.rules
      in
      { rules         = machine.rules;
        state         = state;
        halting_state = machine.halting_state;
        tape          = Tape.step symbol direction machine.tape }
    with
      | Not_found -> raise Diverged


(*
  Current state.
*)
let state machine =
  machine.state


(*
  Get contents of the tape as a triple:
  - list of symbols before the reading head
  - symbol at the reading head
  - list of symbols after the reading head
*)
let tape machine =
  Tape.contents machine.tape

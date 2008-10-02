(*
  Implementation of a turing machine.
*)


type machine = Program.program * Tape.tape * Program.state

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

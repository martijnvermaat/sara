type state         = string
type configuration = state * Tape.symbol
type action        = state * Tape.symbol * Tape.direction
type rule          = configuration * action
type program

exception Halted
exception Diverged

val parse             : string -> program
val pretty_print_rule : rule -> string
val pretty_print      : program -> string * string * string
val initial_state     : program -> state
val step              : program -> configuration -> action

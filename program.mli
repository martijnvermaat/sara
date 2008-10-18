type state         = string
type configuration = state * Tape.symbol
type action        = state * Tape.symbol * Tape.direction
type rule          = configuration * action
type rules
type program

exception Halted
exception Diverged

val create            : rules -> state -> state -> program
val parse             : string -> program
val parse_rules       : string -> rules
val pretty_print_rule : rule -> string
val pretty_print      : program -> string * string * string
val initial_state     : program -> state
val step              : program -> configuration -> action

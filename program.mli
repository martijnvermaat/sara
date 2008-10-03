type state         = string
type configuration = state * Tape.symbol
type action        = state * Tape.symbol * Tape.direction
type rule          = configuration * action
type program

exception Halted
exception Diverged

val parse         : string -> program
val initial_state : program -> state
val step          : program -> configuration -> action

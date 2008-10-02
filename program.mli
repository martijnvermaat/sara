type state         = string
type configuration = state * Tape.symbol
type action        = state * Tape.symbol * Tape.direction
type rule          = configuration * action
type program

exception Halted
exception Diverged

val parse : string -> program
val step  : program -> configuration -> action

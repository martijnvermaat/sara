type state   = string
type rule    = { current_state  : state;
                 current_symbol : Tape.symbol;
                 new_state      : state;
                 new_symbol     : Tape.symbol;
                 direction      : Tape.direction }
type machine

exception Halted
exception Diverged

val create : rule list -> state -> state -> Tape.symbol list -> machine

val step : machine -> machine

val state : machine -> state

val tape : machine -> (Tape.symbol list * Tape.symbol * Tape.symbol list)

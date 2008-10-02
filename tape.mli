type symbol    = int option
type direction = Left | Right
type tape

val parse    : string -> tape
val step     : tape -> symbol -> direction -> tape
val read     : tape -> symbol
val contents : tape -> (symbol list * symbol * symbol list)

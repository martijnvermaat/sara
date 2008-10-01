%token <string> ID
%token EOL
%start main
%type <Machine.rule> main
%%
main:
  expr EOL { $1 }
;
expr:
  state symbol state symbol direction { { Machine.current_state=$1; Machine.current_symbol=$2; Machine.new_state=$3; Machine.new_symbol=$4; Machine.direction=$5 } }
;
state:
  ID { $1 }
;
symbol:
  ID {
    if $1 = "B" then
      None
    else
      try
        Some (int_of_string $1)
      with
        | Failure _ -> raise Parsing.Parse_error
  }
;
direction:
  ID {
    match $1 with
      | "l"     -> Tape.Left
      | "r"     -> Tape.Right
      | "left"  -> Tape.Left
      | "right" -> Tape.Right
      | _       -> raise Parsing.Parse_error
  }

%token <string> ID
%token EOL EOF
%start program rules
%type <Program.rule list * Program.state * Program.state> program
%type <Program.rule list> rules
%%
program:
  state state EOL rules EOF { ( $4, $1, $2 ) }
;
rules:
                   { [] }
  | rules rule EOL { $2 :: $1 }
;
rule:
  state symbol state symbol direction { ( ($1, $2), ($3, $4, $5) ) }
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

(*
  Graphical program for running turing machines.
*)


class program interface = object (self : 'self)

  val interface = interface
  val mutable tape = None
  val mutable program = None
  val mutable state = None

  method load_tape string =
    try
      let t = Tape.parse string in
      tape <- Some t;
      interface#on_tape_changed (Tape.contents t)
    with
      | Failure _ -> ()

  method load_program rules initial_state halting_state =
    try
      let r = Program.parse_rules rules in
      program <- Some (Program.create r initial_state halting_state);
      state <- Some initial_state
    with
      | Failure _ -> ()

  method load_program_file file =
    let read_file file =
      let channel = open_in file in
      let size = in_channel_length channel in
      let buffer = String.create size in
      really_input channel buffer 0 size;
      close_in channel;
      buffer
    in
    try
      let p = Program.parse (read_file file) in
      program <- Some p;
      state <- Some (Program.initial_state p)
    with
      | Failure _ -> ()

  method step () =
    match tape, program, state with
      | Some t, Some p, Some s ->
          begin try
            let new_state, symbol, direction =
              Program.step p (s, Tape.read t)
            in
            let new_tape = Tape.step t symbol direction
            in
            tape <- Some new_tape;
            state <- Some new_state;
            interface#on_tape_changed (Tape.contents new_tape);
            interface#on_state_changed new_state
          with
            | Program.Diverged -> () (*status_message "Program diverged"*)
            | Program.Halted   -> () (*status_message "Program halted"*)
          end
      | _ -> () (*status_message "No tape or program loaded"*)

  method run =
    ignore interface#run

  initializer
    (*interface#connect_load_tape self#load_tape;
    interface#connect_open_program self#open_program*)
    interface#connect_step self#step

end


let main () =

  let program  = ref None
  and tape     = ref ""
  in

  let arguments = Arg.align
    [("-p", Arg.String (fun a -> program := Some a),
      " Program")]
  and description = "./turing [-p program] [tape]"
  in

  Arg.parse arguments (fun a -> tape := a) description;

  let p = new program (new Graphical.interface)
  and tape_string = !tape in
  p#load_tape tape_string;
  begin match !program with
    | Some program_file -> p#load_program_file program_file
    | None              -> ()
  end;
  p#run


(*
  Start main program.
*)
let _ = main ()

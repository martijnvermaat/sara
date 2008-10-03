(*
  Graphical program for running turing machines.
*)


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

  let tape_string = !tape in
  match !program with
    | Some program_file -> Ui.main ~program_file ~tape_string ()
    | None              -> Ui.main ~tape_string ()


(*
  Start main program.
*)
let _ = main ()

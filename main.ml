(*
  Graphical program for running turing machines.
*)


let main () =

  let arg_program_file  = ref None
  and arg_tape_string   = ref None
  in

  let arguments = Arg.align
    [("-p", Arg.String (fun a -> arg_program_file := Some a),
      " Program (required)")]
  and description = "./turing -p program tape"
  in

  Arg.parse arguments (fun a -> arg_tape_string := Some a) description;

  match !arg_program_file, !arg_tape_string with
    | Some program_file, Some tape_string ->
        Ui.main program_file tape_string
    | _ ->
        Arg.usage arguments description;
        exit 1


(*
  Start main program.
*)
let _ = main ()

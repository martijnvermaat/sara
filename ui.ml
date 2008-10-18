(*
  Graphical program for running turing machines.
*)


(* TODO: exceptions *)
let read_file file =
  let channel = open_in file in
  let size = in_channel_length channel in
  let buffer = String.create size in
  really_input channel buffer 0 size;
  close_in channel;
  buffer


let create_rules_view packing =
  let buffer = GSourceView.source_buffer () in
  let view = GSourceView.source_view
    ~source_buffer:buffer ~packing ()
  in
  view#misc#modify_font_by_name "Monospace";
  view


let main ?program_file ?(tape_string="") () =

  let window = new Widgets.main_window () in
  let tape_view = window#tape
  and rules_view = create_rules_view window#rules_scroller#add
  and status_context = window#status#new_context ""
  and tape = ref None
  and saved_tape_string = ref ""
  and program_state = ref None
  and steps = ref 0
  in

  let update_ui () =
    begin match !program_state with
      | None ->
          window#button_step#misc#set_sensitive false;
          window#button_run#misc#set_sensitive false;
          window#button_reset#misc#set_sensitive false;
          window#current_state#set_text "No state"
      | Some (_, state) ->
          window#button_step#misc#set_sensitive true;
          window#button_run#misc#set_sensitive true;
          window#button_reset#misc#set_sensitive true;
          window#current_state#set_text state
    end;
    window#steps#set_label (string_of_int !steps);
    GtkBase.Widget.queue_draw window#tape#as_widget

  and status_message string =
    status_context#flash ~delay:3000 string

  and apply_program_activation value =
    window#button_apply_program#misc#set_sensitive value
  in

  (* TODO: errors *)
  let load_program () =
    let old_program_state = !program_state
    in
    program_state := begin
      try
        let initial_state = window#initial_state#text
        and halting_state = window#halting_state#text
        and rules = Program.parse_rules (rules_view#source_buffer#get_text ()) in
        apply_program_activation false;
        status_message "Program loaded";
        match old_program_state with
          | None ->
              steps := 0;
              Some ((Program.create rules initial_state halting_state), initial_state)
          | Some (_, state) ->
              Some ((Program.create rules initial_state halting_state), state)
      with
        | Failure _ -> None
    end;
    update_ui ()

  (* TODO: errors *)
  and load_tape string =
    begin try
      tape := Some (Tape.parse string);
      saved_tape_string := string;
      status_message "Tape loaded"
    with
      | Failure _ -> ()
    end;
    update_ui ()
  in

  let reset_program () =
    begin match !program_state with
      | None -> ()
      | Some (program, state) ->
          steps := 0;
          program_state := Some (program, Program.initial_state program);
          load_tape !saved_tape_string;
          window#tape_text#set_text !saved_tape_string (* TODO: do this or not? *)
    end;
    status_message "Program reset to initial state";
    update_ui ()

  and tape_view_expose _ =
    Draw.draw_tape (!tape) tape_view;
    false
  in

  let step _ =
    match !tape, !program_state with
      | Some tape', Some (program, state) ->
          begin try
            let new_state, symbol, direction =
              Program.step program (state, Tape.read tape')
            in
            let new_tape = Tape.step tape' symbol direction
            in
            tape := Some new_tape;
            program_state := Some (program, new_state);
            incr(steps);
            status_message "Program executed one step";
            update_ui ();
            true
          with
            | Program.Diverged -> status_message "Program diverged"; false
            | Program.Halted   -> status_message "Program halted"; false
          end
      | _ ->
          status_message "No tape or program loaded";
          false
  in

  let load_program_text program =
    let rules, initial_state, halting_state =
      Program.pretty_print (Program.parse program)
    in
    window#initial_state#set_text initial_state;
    window#halting_state#set_text halting_state;
    rules_view#source_buffer#set_text rules
  in

  let run _ =
    ignore (GMain.Timeout.add ~ms:300 ~callback:step)

  and open_program _ =
    let file_chooser = GWindow.file_chooser_dialog
      ~action:`OPEN
      ~parent:window#toplevel
      ~destroy_with_parent:true
      ~title:"Open program" ()
    in
    file_chooser#add_button_stock `CANCEL `CANCEL ;
    file_chooser#add_select_button_stock `OPEN `OPEN ;
    begin match file_chooser#run () with
      | `OPEN ->
          begin match file_chooser#filename with
            | Some s ->
                load_program_text (read_file s);
                load_program ();
                reset_program ();
                status_message ("Opened program " ^ s);
            | None   -> ()
          end
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    file_chooser#destroy ()

  and edit_tape _ =
    load_program ();
    load_tape window#tape_text#text
  in

  ignore (window#tape#event#connect#expose tape_view_expose);
  ignore (window#menu_open_program#connect#activate open_program);
  ignore (window#button_edit_tape#connect#clicked edit_tape);
  ignore (window#button_apply_program#connect#clicked load_program);
  ignore (window#button_reset#connect#clicked reset_program);
  ignore (window#button_step#connect#clicked (fun _ -> ignore (step ())));
  ignore (window#button_run#connect#clicked run);

  ignore (window#toplevel#connect#destroy GMain.quit);
  ignore (window#menu_quit#connect#activate GMain.quit);
  ignore (window#toplevel#event#connect#delete (fun _ -> GMain.quit (); true));

  ignore (window#current_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (window#initial_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (window#initial_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (rules_view#source_buffer#connect#changed (fun _ -> apply_program_activation true));

  let font = Pango.Font.from_string "Monospace" in
  window#tape_text#misc#modify_font font;

  apply_program_activation false;

  begin match program_file with
    | None      -> ()
    | Some file -> load_program_text (read_file file)
  end;

  load_tape tape_string;
  load_program ();
  window#tape_text#set_text tape_string;

  window#toplevel#show ();
  GMain.Main.main ()

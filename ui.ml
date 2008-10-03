(*
  Graphical program for running turing machines.
*)


(* Why does OCaml not know about Pi? *)
let pi = 4. *. atan 1.


(*
  Draw a representation of [tape] on [area].
*)
let draw_tape tape (area : GMisc.drawing_area) =

  (* TODO: clean up *)
  let tape = match tape with
    | None   -> [], None, []
    | Some t -> Tape.contents t
  in

  (*
    The current height of [area] is used. We horizontally resize [area] if we
    need more space.
    We draw the part of the tape that contains non-blank cells and a number
    of extra blank cells on both ends.

    TODO: Use margins of widget
  *)

  let num_extra_cells = 3    (* Number of extra blank cells on each end *)
  and line_width      = 2.   (* Width of tape lines *)
  in

  let cells_before, current_cell, cells_after = tape in
  let cells = cells_before @ (current_cell :: cells_after) in

  (* Width and height of [area] *)
  let { Gtk.width = width ; Gtk.height = height } = area#misc#allocation in

  (* Size of a cell *)
  let cell_width  = (float height) /. 2.1
  and cell_height = (float height) /. 2.1
  in

  (* Font properties *)
  let font_size = cell_height /. 2.
  and font_face = "sans"
  in

  (* Margins of [area] *)
  let margin_left   = cell_width *. 0.25
  and margin_right  = cell_width *. 0.25
  and margin_top    = cell_height *. 0.25
  in

  (* Some convenient numbers *)
  let extra_cells_width = float num_extra_cells *. cell_width
  and tape_width =
    float (List.length cells + num_extra_cells * 2) *. cell_width
  and reading_head =
    margin_left
    +. (float (num_extra_cells + (List.length cells_before)) +. 0.5)
    *. cell_width
  in

  (* New required width of [area] *)
  let width = int_of_float (ceil (margin_left +. tape_width +. margin_right))
  in

  (* Request new width of [area] *)
  area#misc#set_size_request ~width ();

  (* Create Cairo context on [area] *)
  let ctx = Cairo_lablgtk.create area#misc#window in

  (* White background *)
  (* TODO: Use gtk defined colors *)
  (* Cairo_lablgtk.set_source_color ctx (GDraw.color `WHITE); *)
  Cairo.set_source_rgb ctx 1. 1. 1.;
  Cairo.paint ctx;

  (* Width of tape lines *)
  Cairo.set_line_width ctx line_width;

  (*
    We first draw the extra cells at the left end, and fade them out to the
    left. Likewise for the right end.
    After that we draw the used cells in between.
  *)

  (* Prepare left fadeout mask *)
  let fade_left = Cairo.Pattern.create_linear
    ~x0:margin_left ~y0:0.
    ~x1:(extra_cells_width +. margin_left) ~y1:0.
  in
  Cairo.Pattern.add_color_stop_rgba fade_left ~off:0. ~red:0. ~green:0. ~blue:0. ~alpha:0.;
  Cairo.Pattern.add_color_stop_rgba fade_left ~off:1. ~red:0. ~green:0. ~blue:0. ~alpha:1.;
  Cairo.set_source ctx fade_left;

  (* Draw left end of tape *)
  Cairo.move_to ctx margin_left margin_top;
  Cairo.rel_line_to ctx extra_cells_width 0.;
  Cairo.move_to ctx margin_left (margin_top +. cell_height);
  Cairo.rel_line_to ctx extra_cells_width 0.;

  (* Draw left cell delimiters *)
  Cairo.move_to ctx (margin_left +. cell_width) margin_top;
  for i = 1 to num_extra_cells do
    Cairo.rel_line_to ctx 0. cell_height;
    Cairo.rel_move_to ctx cell_width (-. cell_height)
  done;

  (* Apply the ink *)
  Cairo.stroke ctx ;

  (* Prepare right fadeout mask *)
  let fade_right = Cairo.Pattern.create_linear
    ~x0:(margin_left +. tape_width) ~y0:0.
    ~x1:(margin_left +. tape_width -. extra_cells_width) ~y1:0.
  in
  Cairo.Pattern.add_color_stop_rgba fade_right ~off:0.
    ~red:0. ~green:0. ~blue:0. ~alpha:0.;
  Cairo.Pattern.add_color_stop_rgba fade_right ~off:1.
    ~red:0. ~green:0. ~blue:0. ~alpha:1.;
  Cairo.set_source ctx fade_right;

  (* Draw right end of tape *)
  Cairo.move_to ctx (margin_left +. tape_width) margin_top;
  Cairo.rel_line_to ctx (-. extra_cells_width) 0.;
  Cairo.move_to ctx (margin_left +. tape_width) (margin_top +. cell_height);
  Cairo.rel_line_to ctx (-. extra_cells_width) 0.;

  (* Draw right cell delimiters *)
  Cairo.move_to ctx (margin_left +. tape_width -. cell_width) margin_top;
  for i = 1 to num_extra_cells do
    Cairo.rel_line_to ctx 0. cell_height;
    Cairo.rel_move_to ctx (-. cell_width) (-. cell_height)
  done;

  (* Apply the ink *)
  Cairo.stroke ctx ;

  (* The rest is painted solidly *)
  Cairo.set_source_rgb ctx 0. 0. 0.;

  (* Draw tape *)
  Cairo.move_to ctx
    (margin_left +. extra_cells_width)
    margin_top;
  Cairo.line_to ctx
    (margin_left +. tape_width -. extra_cells_width)
    margin_top;
  Cairo.move_to ctx
    (margin_left +. extra_cells_width)
    (margin_top +. cell_height);
  Cairo.line_to ctx
    (margin_left +. tape_width -. extra_cells_width)
    (margin_top +. cell_height);

  (* Draw cell delimiters *)
  Cairo.move_to ctx (margin_left +. extra_cells_width) margin_top;
  for i = 1 to (List.length cells) - 1 do
    Cairo.rel_move_to ctx cell_width cell_height;
    Cairo.rel_line_to ctx 0. (-. cell_height);
  done;

  (* Apply the ink *)
  Cairo.stroke ctx;

  (* Draw digits *)
  Cairo.select_font_face ctx
    font_face
    Cairo.FONT_SLANT_NORMAL
    Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_font_size ctx font_size;

  let fe = Cairo.font_extents ctx in
  let y_offset = fe.Cairo.descent -. fe.Cairo.font_height /. 2.
  in

  for i = 0 to (List.length cells) - 1 do
    Cairo.move_to ctx
      (margin_left +. extra_cells_width +. (float i +. 0.5) *. cell_width)
      (margin_top +. cell_height /. 2. -. y_offset);
    match List.nth cells i with
      | None   -> ()
      | Some n ->
          let s = string_of_int n in
          let te = Cairo.text_extents ctx s in
          let x_offset = te.Cairo.x_bearing +. te.Cairo.text_width /. 2. in
          Cairo.rel_move_to ctx (-. x_offset) 0.;
          Cairo.show_text ctx s;
  done;

  (* Reading head *)
  (* TODO: Make this nicer *)
  Cairo.move_to ctx reading_head (margin_top +. cell_height *. 1.125);
  Cairo.rel_line_to ctx (-. cell_width /. 4.) (cell_height /. 2.);
  Cairo.rel_line_to ctx (cell_width /. 2.) 0.;
  Cairo.close_path ctx;
  Cairo.stroke_preserve ctx;
  Cairo.set_source_rgb ctx 0.5 0.5 0.9;
  Cairo.fill ctx;
  Cairo.set_source_rgb ctx 1. 1. 1.;
  Cairo.arc ctx
    reading_head (margin_top +. cell_height *. 1.125)
    (cell_width /. 8.) 0.
    (2. *. pi);
  Cairo.fill_preserve ctx;
  Cairo.set_source_rgb ctx 0. 0. 0.;
  Cairo.stroke ctx;

  (* Draw small symbol of current cell *)
  Cairo.set_font_size ctx (font_size /. 2.5);
  let s = match current_cell with
    | None   -> "B"
    | Some n -> string_of_int n
  in
  let fe = Cairo.font_extents ctx in
  let te = Cairo.text_extents ctx s in
  let x_offset = te.Cairo.x_bearing +. te.Cairo.text_width /. 2.
  and y_offset = fe.Cairo.descent -. fe.Cairo.font_height /. 2.
  in
  Cairo.move_to ctx
    (reading_head -. x_offset)
    (margin_top +. cell_height *. 1.125 -. y_offset);
  Cairo.show_text ctx s


(* TODO: exceptions *)
let read_file file =
  let channel = open_in file in
  let size = in_channel_length channel in
  let buffer = String.create size in
  really_input channel buffer 0 size;
  close_in channel;
  buffer


let create_program_view packing =
  let buffer = GSourceView.source_buffer () in
  let view = GSourceView.source_view
    ~source_buffer:buffer ~packing ()
  in
  view#misc#modify_font_by_name "Monospace";
  view


let main ?program_file ?(tape_string="") () =

  let window = new Widgets.main_window () in
  let tape_view = window#tape
  and program_view = create_program_view window#program_scroller#add
  and tape = ref None
  and program_state = ref None
  and steps = ref 0
  in

  let load_program () =
    program_state :=
      try
        let program =
          Program.parse (program_view#source_buffer#get_text ())
        in
        Some (program, Program.initial_state program)
      with
        | Failure _ -> None

  and load_tape string =
    tape :=
      try
        Some (Tape.parse string)
      with
        | Failure _ -> None

  and update_ui () =
    window#state#set_label
      begin match !program_state with
        | Some (_, state) -> state
        | None            -> "No state"
      end;
    window#steps#set_label (string_of_int !steps);
    GtkBase.Widget.queue_draw window#tape#as_widget

  and tape_view_expose _ =
    draw_tape (!tape) tape_view;
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
            update_ui ()
          with
            | Program.Diverged -> print_endline "Reached a deadlock"
            | Program.Halted   -> print_endline "Halted"
          end
      | _ ->
          print_endline "No tape or program loaded"

  and run _ =
    (*
    match !tape, !program_state with
      | Some tape', Some (program, state) ->
          begin try
            while true do
              let new_state, symbol, direction =
                Program.step program (state, Tape.read tape')
              in
              let new_tape = Tape.step tape' symbol direction
              in
              tape := Some new_tape;
              program_state := Some (program, new_state);
              incr(steps);

              machine := Machine.step !machine;
              steps := !steps + 1;
            done
          with
            | Machine.Diverged -> print_endline "Reached a deadlock"
            | Machine.Halted   ->
                print_endline "Halted";
                window#state#set_label (Machine.state !machine);
                window#steps#set_label (string_of_int !steps);
                GtkBase.Widget.queue_draw window#tape#as_widget
    *)
    ()

  and open_program _ =
    (*
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
            | Some s -> show_program s source_buffer
            | None   -> ()
          end
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    file_chooser#destroy ()
    *)
    ()
  in

  ignore (window#tape#event#connect#expose tape_view_expose);
  ignore (window#button_open#connect#clicked open_program);
  ignore (window#button_step#connect#clicked step);
  ignore (window#button_run#connect#clicked run);

  ignore (window#toplevel#connect#destroy GMain.quit);
  ignore (window#toplevel#event#connect#delete (fun _ -> GMain.quit (); true));

  begin match program_file with
    | None -> ()
    | Some file ->
        program_view#source_buffer#set_text (read_file file)
  end;

  load_program ();
  load_tape tape_string;

  window#toplevel#show ();
  GMain.Main.main ()

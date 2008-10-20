(*
  Graphical program fng machines.
*)


class interface = object (self : 'self)
  inherit Widgets.main_window ()

  val rules =
    let buffer = GSourceView.source_buffer () in
    GSourceView.source_view ~source_buffer:buffer ()

  val mutable tape_contents = [], None, []

  method rules = rules

  method on_tape_changed tape =
    tape_contents <- tape;
    GtkBase.Widget.queue_draw self#tape#as_widget

  method on_state_changed state =
    self#current_state#set_text state

  method run =
    self#toplevel#show ();
    GMain.Main.main ()

  method connect_step step =
    ignore (self#button_step#connect#clicked (fun _ -> step ()))

  initializer
    self#rules_scroller#add self#rules#coerce;
    rules#misc#modify_font_by_name "Monospace";

    ignore (self#tape#event#connect#expose (fun _ -> begin Draw.draw_tape tape_contents self#tape; false end));

(*  ignore (self#menu_open_program#connect#activate open_program);
  ignore (self#button_edit_tape#connect#clicked edit_tape);
  ignore (self#button_apply_program#connect#clicked load_program);
  ignore (self#button_reset#connect#clicked reset_program);
  ignore (self#button_step#connect#clicked (fun _ -> ignore (step ())));
  ignore (self#button_run#connect#clicked run);*)

    ignore (self#toplevel#connect#destroy GMain.quit);
    ignore (self#menu_quit#connect#activate GMain.quit);
    ignore (self#toplevel#event#connect#delete (fun _ -> GMain.quit (); true));

(*  ignore (self#current_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (self#initial_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (self#initial_state#connect#changed (fun _ -> apply_program_activation true));
  ignore (self#rules#source_buffer#connect#changed (fun _ -> apply_program_activation true));*)

    let font = Pango.Font.from_string "Monospace" in
    self#tape_text#misc#modify_font font;

end

OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

LAYOUT = layout.glade
MODEL = tape.mli tape.ml machine.mli machine.ml
UTIL = programParser.mly programLexer.mll util.ml ui.ml
MAIN = main.ml

SOURCES = $(LAYOUT) $(MODEL) $(UTIL) $(MAIN)
RESULT = sara
INCDIRS = +cairo +lablgtk2
CLIBS = mlcairo
OCAMLBLDFLAGS = lablgtk.cma gtkInit.cmo lablglade.cma cairo.cma cairo_lablgtk.cma lablgtksourceview.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablglade.cmxa cairo.cmxa cairo_lablgtk.cmxa lablgtksourceview.cmxa

-include $(OCAMLMAKEFILE)

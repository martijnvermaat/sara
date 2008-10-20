OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile

LAYOUT = widgets.glade
INCLUDE = tape.mli tape.ml programParser.mly programLexer.mll program.mli program.ml draw.ml graphical.ml
MAIN = main.ml

SOURCES = $(LAYOUT) $(INCLUDE) $(MAIN)
RESULT = sara
INCDIRS = +cairo +lablgtk2
CLIBS = mlcairo
OCAMLBLDFLAGS = lablgtk.cma gtkInit.cmo lablglade.cma cairo.cma cairo_lablgtk.cma lablgtksourceview.cma
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx lablglade.cmxa cairo.cmxa cairo_lablgtk.cmxa lablgtksourceview.cmxa

-include $(OCAMLMAKEFILE)

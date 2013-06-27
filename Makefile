FILES1=Timer.cmx Helpers.cmx OstapLexer.cmx Lang.cmx  \
	Printer.cmx Lang2.cmx JavaOstap.cmx JavaYacc.cmx PrinterYacc.cmx
FILES_YACC=Lexer.cmx Parser.cmx

OCAMLC=ocamlfind opt -rectypes
OPTIONS=-thread -package typeutil,settings,checked,ostap -I `ocamlc -where`/camlp5
OUT=a.out

.SUFFIXES: .cmx .cmi .ml .mly .mll .mli

all: $(FILES1) with_lex Driver.cmx Driver2.cmx
		$(OCAMLC) $(OPTIONS) -linkpkg str.cmxa ostap.cmx $(FILES1) $(FILES_YACC) Driver.cmx -o $(OUT)
		$(OCAMLC) $(OPTIONS) -linkpkg str.cmxa ostap.cmx $(FILES1) $(FILES_YACC) Driver2.cmx -o test2

with_lex:
		ocamlyacc Parser.mly
		$(OCAMLC) $(OPTIONS) -i Parser.ml > Parser.mli
		$(OCAMLC) $(OPTIONS) -c Parser.mli
		$(OCAMLC) $(OPTIONS) -c Parser.ml
		ocamllex Lexer.mll
		$(OCAMLC) $(OPTIONS) -c Lexer.ml

.ml.cmx:
		$(OCAMLC) $(OPTIONS) -c -pp "camlp5o pa_checked.cmo pa_ostap.cmo pa_log.cmo" $<

.mli.cmi:
		$(OCAMLC) -c $<

clean:
		rm -fr *~ *.cm[oixa] $(OUT) _build Lexer.ml Parser.ml Parser.mli *.o HelloWorld.class

j:
		jasmin HelloWorld.j && java HelloWorld

ob:
		ocamlbuild -use-ocamlfind Driver.byte


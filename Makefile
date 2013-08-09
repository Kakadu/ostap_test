FILES0=Timer.cmx Helpers.cmx PrinterYacc.cmx OstapExprPrinter.cmx
FILES1=OstapLexer.cmx OstapLexerExpr.cmx Lang.cmx Printer.cmx  \
	 ExprOstap2.cmx ExprOstap3.cmx \
	   \
	 Lang2.cmx JavaOstap.cmx JavaYacc.cmx
FILES_YACC=Lexer.cmx Parser.cmx LexerExpr.cmx ExprYacc.cmx

OCAMLC=ocamlfind opt -rectypes -p -S -g
OPTIONS=-thread -package typeutil,settings,checked,ostap -I `ocamlc -where`/camlp5
OUT=a.out

.SUFFIXES: .cmx .cmi .ml .mly .mll .mli

all: $(FILES0) with_lex $(FILES1)  Driver.cmx Driver2.cmx  TestExpr.cmx
#		$(OCAMLC) $(OPTIONS) -linkpkg $(FILES0)  $(FILES1) $(FILES_YACC) Driver.cmx -o $(OUT)
#		$(OCAMLC) $(OPTIONS) -linkpkg $(FILES0)  $(FILES1) $(FILES_YACC) Driver2.cmx -o test2
		$(OCAMLC) $(OPTIONS) -linkpkg $(FILES0)  $(FILES_YACC) $(FILES1)  TestExpr.cmx -o testExpr

with_lex:
		ocamlyacc Parser.mly
		ocamlyacc ExprYacc.mly
		$(OCAMLC) $(OPTIONS) -i Parser.ml > Parser.mli
		$(OCAMLC) $(OPTIONS) -i ExprYacc.ml > ExprYacc.mli
		$(OCAMLC) $(OPTIONS) -c Parser.mli
		$(OCAMLC) $(OPTIONS) -c Parser.ml
		$(OCAMLC) $(OPTIONS) -c ExprYacc.mli
		$(OCAMLC) $(OPTIONS) -c ExprYacc.ml
		ocamllex Lexer.mll
		ocamllex LexerExpr.mll
		$(OCAMLC) $(OPTIONS) -c Lexer.ml
		$(OCAMLC) $(OPTIONS) -c LexerExpr.ml

.ml.cmx:
		$(OCAMLC) $(OPTIONS) -c -pp "camlp5o  pa_macro.cmo pa_checked.cmo pa_ostap.cmo pa_log.cmo -DMEM" $<

.mli.cmi:
		$(OCAMLC) -c $<

clean:
		rm -fr *~ *.s *.cm[oixa] $(OUT) _build Lexer.ml Parser.ml Parser.mli *.o HelloWorld.class LexerExpr.ml ExprYacc.ml ExprYacc.mli

j:
		jasmin HelloWorld.j && java HelloWorld

ob:
		ocamlbuild -use-ocamlfind Driver.native Driver2.native TestExpr.native


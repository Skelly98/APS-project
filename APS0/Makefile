LEX_ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc

#eval: parser eval.ml
#	$(OCAMLC) -o eval ast.cmo lexer.cmo parser.cmo eval.ml

toProlog: parser toProlog.ml
	$(OCAMLC) -o toProlog ast.cmo lexer.cmo parser.cmo toProlog.ml


parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f toProlog
#	rm -f eval
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml

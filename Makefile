
all:
	ocamllex sdllexer.mll
	ocamlyacc sdlparser.mly
	ocamlc -c path.mli
	ocamlc -c path.ml
	ocamlc -c sdlparser.mli 
	ocamlc -c sdllexer.ml
	ocamlc -c sdlparser.ml
	ocamlc -c main.ml
	ocamlc -o sdlc sdllexer.cmo sdlparser.cmo path.cmo main.cmo

clean:
	rm sdlc *.cmo sdllexer.ml sdlparser.ml sdlparser.mli *.cmi

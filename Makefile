
all: objs
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o si lexer.cmo parser.cmo interpreterObjects.cmo main.cmo

objsInterface:
	ocamlc -i interpreterObjects.ml > interpreterObjects.mli
	ocamlc -c interpreterObjects.mli

objs: objsInterface
	ocamlc -c interpreterObjects.ml

clean:
	rm si *.cmo *.cmi lexer.ml parser.ml parser.mli interpreterObjects.mli

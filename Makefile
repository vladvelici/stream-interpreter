# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file sdlc
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule 
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between 
#                  existing modules.  (The graph is stored in the file
#                  .depend)

# These are the object files needed to rebuild the main executable file
#
OBJS = interpreterObjects.cmo parser.cmo lexer.cmo printer.cmo environment.cmo exceptions.ml typechecker.ml native.cmo eval2.cmo main.cmo

# Files that need to be generated from other files
DEPEND += lexer.ml parser.ml interpreterObjects.mli 

# When "make" is invoked with no arguments, we build an executable 
# typechecker, after building everything that it depends on
all: .depend $(DEPEND) $(OBJS) runinterp

# Include an automatically generated list of dependencies between source files
# (if something doesn't compile, run make depend)
-include .depend

interpreterObjects.mli:
	ocamlc -i interpreterObjects.ml > interpreterObjects.mli

# Build an executable typechecker
runinterp: $(OBJS) main.cmo 
	@echo Linking $@
	ocamlc -o $@ $(COMMONOBJS) $(OBJS) 

# Compile an ML module interface
%.cmi : %.mli
	ocamlc -c $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc -c $<

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf interpreterObjects.mli lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
	   runinterp TAGS *~ .depend

# Rebuild intermodule dependencies
.depend:: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend

# 

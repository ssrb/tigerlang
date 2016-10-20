RESULT = lexer_tester
SOURCES = lexer/tigerlex.mll lexer/lexer_tester.ml
OCAMLYACC = menhir
BCSUFFIX = ".byte"

all: debug-code native-code

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

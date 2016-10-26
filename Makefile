OCAMLMAKEFILE = OCamlMakefile

export BCSUFFIX = ".byte"
export OCAMLYACC = menhir
export PACKS = core
export THREADS = yes

define PROJ_lexer_tester
	RESULT = lexer/lexer_tester
	SOURCES = lexer/tigerlex.mll lexer/tokens.ml lexer/testtokens.ml lexer/lexer_tester.ml
endef
export PROJ_lexer_tester

define PROJ_parser_tester
	RESULT = parser/parser_tester
	SOURCES = parser/tigerparse.mly lexer/tigerlex.mll
	YFLAGS = --infer --explain --trace
endef
export PROJ_parser_tester

ifndef SUBPROJS
  export SUBPROJS = lexer_tester parser_tester
endif

all: debug-code native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
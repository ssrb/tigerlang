OCAMLMAKEFILE = OCamlMakefile

export BCSUFFIX = ".byte"
export OCAMLYACC = menhir

define PROJ_lexer_tester
	RESULT = lexer/lexer_tester
	SOURCES = lexer/tigerlex.mll lexer/lexer_tester.ml
endef
export PROJ_lexer_tester

define PROJ_parser_tester
	RESULT = parser/parser_tester
	SOURCES = parser/tigerparse.mly lexer/tigerlex.mll
endef
export PROJ_parser_tester

ifndef SUBPROJS
  export SUBPROJS = lexer_tester parser_tester
endif

all: debug-code native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
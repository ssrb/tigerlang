OCAMLMAKEFILE = OCamlMakefile

export BCSUFFIX = ".byte"
export OCAMLYACC = menhir
export PACKS = core
export THREADS = yes

define PROJ_lexer_tester
	RESULT = lexer/lexer_tester
	SOURCES = lexer/tigerlex_intf.ml lexer/tigerlex.mll lexer/testtokens.ml lexer/lexer_tester.ml
endef
export PROJ_lexer_tester

define PROJ_parser_tester
	RESULT = parser/parser_tester
	SOURCES = absyn/absyn.ml absyn/table.ml absyn/symbol.ml parser/tigerparse.mly lexer/tigerlex_intf.ml lexer/tigerlex.mll parser/parsertokens.ml parser/parser_tester.ml
	YFLAGS = --explain --trace --dump
endef
export PROJ_parser_tester

define PROJ_absyn_tester
	RESULT = absyn/absyn_tester
	SOURCES = absyn/absyn.ml absyn/table.ml absyn/symbol.ml parser/tigerparse.mly lexer/tigerlex_intf.ml lexer/tigerlex.mll parser/parsertokens.ml absyn/prabsyn.ml absyn/absyn_tester.ml
	YFLAGS = --explain --trace --dump
endef
export PROJ_absyn_tester

define PROJ_semant_tester
	RESULT = semant/semant_tester
	SOURCES = absyn/absyn.ml absyn/table.ml absyn/symbol.ml parser/tigerparse.mly lexer/tigerlex_intf.ml lexer/tigerlex.mll parser/parsertokens.ml semant/types.ml semant/translate.mli semant/env.ml semant/types.mli semant/semant.ml
	YFLAGS = --explain --trace --dump
endef
export PROJ_semant_tester

ifndef SUBPROJS
  export SUBPROJS = lexer_tester parser_tester absyn_tester semant_tester
endif

all: debug-code native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
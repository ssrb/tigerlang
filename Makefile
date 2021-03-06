OCAMLMAKEFILE = OCamlMakefile

export BCSUFFIX = ".byte"
export OCAMLYACC = menhir
export PACKS = base core ppx_sexp_conv ppx_compare glpk
export THREADS = yes
export OCAMLFLAGS = -open Base

define PROJ_lexer_tester
	RESULT = lexer/lexer_tester
	SOURCES = lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	lexer/testtokens.ml \
	lexer/lexer_tester.ml
endef
export PROJ_lexer_tester

define PROJ_parser_tester
	RESULT = parser/parser_tester
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	parser/parser_tester.ml
#	YFLAGS = --explain --trace --dump
endef
export PROJ_parser_tester

define PROJ_semant_tester
	RESULT = semant/semant_tester
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	semant/types.ml \
	irtrans/temp.ml \
	irtrans/tree.ml \
	iselect/assem.ml \
	irtrans/frame.ml \
	irtrans/translate.ml \
	irtrans/findEscape.ml \
	semant/env.ml \
	semant/semant.ml \
	irtrans/m68kTemp.ml \
	irtrans/m68kFrame.ml \
	semant/semant_tester.ml
endef
export PROJ_semant_tester

define PROJ_canon_tester
	RESULT = canon/canon_tester
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	semant/types.ml \
	irtrans/temp.ml \
	irtrans/tree.ml \
	iselect/assem.ml \
	irtrans/frame.ml \
	irtrans/translate.ml \
	irtrans/findEscape.ml \
	semant/env.ml \
	semant/semant.ml \
	irtrans/m68kTemp.ml \
	irtrans/m68kFrame.ml \
	canon/canon.ml \
	canon/canon_tester.ml
endef
export PROJ_canon_tester

define PROJ_iselect_tester
	RESULT = iselect/iselect_tester
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	semant/types.ml \
	irtrans/temp.ml \
	irtrans/tree.ml \
	iselect/assem.ml \
	irtrans/frame.ml \
	irtrans/translate.ml \
	irtrans/findEscape.ml \
	semant/env.ml \
	semant/semant.ml \
	irtrans/m68kTemp.ml \
	irtrans/m68kFrame.ml \
	canon/canon.ml \
	iselect/codegen.ml \
	iselect/m68kCodegen.ml \
	ml-burg/m68000.ml \
	iselect/iselect_tester.ml
endef
export PROJ_iselect_tester

define PROJ_regalloc_tester
	RESULT = regalloc/regalloc_tester
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	semant/types.ml \
	irtrans/temp.ml \
	irtrans/tree.ml \
	iselect/assem.ml \
	irtrans/frame.ml \
	irtrans/translate.ml \
	irtrans/findEscape.ml \
	semant/env.ml \
	semant/semant.ml \
	irtrans/m68kTemp.ml \
	irtrans/m68kFrame.ml \
	canon/canon.ml \
	iselect/codegen.ml \
	iselect/m68kCodegen.ml \
	ml-burg/m68000.ml \
	regalloc/graph.ml \
	regalloc/flowgraph.ml \
	regalloc/makegraph.ml \
	regalloc/liveness.ml \
	regalloc/color.ml \
	regalloc/regalloc.ml \
	regalloc/regalloc_tester.ml
endef
export PROJ_regalloc_tester

define PROJ_tiger_main
	RESULT = main/tiger
	SOURCES = absyn/table.ml \
	absyn/symbol.ml \
	absyn/absyn.ml \
	parser/tigerparse.mly \
	lexer/tokenCBs.ml \
	lexer/tigerlex.mll \
	parser/parsertokens.ml \
	semant/types.ml \
	irtrans/temp.ml \
	irtrans/tree.ml \
	iselect/assem.ml \
	irtrans/frame.ml \
	irtrans/translate.ml \
	irtrans/findEscape.ml \
	semant/env.ml \
	semant/semant.ml \
	irtrans/m68kTemp.ml \
	irtrans/m68kFrame.ml \
	canon/canon.ml \
	iselect/codegen.ml \
	iselect/m68kCodegen.ml \
	ml-burg/m68000.ml \
	regalloc/graph.ml \
	regalloc/flowgraph.ml \
	regalloc/makegraph.ml \
	regalloc/liveness.ml \
	regalloc/color.ml \
	regalloc/regalloc.ml \
	main/tiger.ml
endef
export PROJ_tiger_main


ifndef SUBPROJS
  export SUBPROJS = lexer_tester parser_tester absyn_tester semant_tester canon_tester iselect_tester regalloc_tester tiger_main
endif

all: ml-burg/m68000.ml debug-code native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

ml-burg/m68000.ml: ml-burg/m68000.burg
	ml-burg/burg ml-burg/m68000.burg
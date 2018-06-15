(* burg-gram
**
** ML-Yacc grammar for BURG.
*)
%{

module A = BurgAST

open Core

let outputRaw s = Out_channel.print_endline (s:string)

%}

%token K_EOF
%token K_TERM
%token K_START
%token K_TERMPREFIX
%token K_RULEPREFIX
%token K_SIG
%token K_COLON
%token K_SEMICOLON
%token K_COMMA
%token K_LPAREN 
%token K_RPAREN
%token K_EQUAL
%token K_PIPE
%token <string list> PPERCENT
%token <int> INT
%token <string> ID 
%token <string list> RAW

(* %nonterm full 		of A.spec_ast
       | spec 		of A.spec_ast
       | decl 		of A.decl_ast
       | binding 	of (string * string option)
       | cost 		of int list
       | costtail 	of int list
       | rulename	of string
       | pattern 	of A.pattern_ast
       | patterntail 	of A.pattern_ast list
       | decls 		of A.decl_ast list
       | rules 		of A.rule_ast list
       | rule 		of A.rule_ast
       | bindinglist 	of (string * string option) list
       | raw	 	of unit
       | prelude	of unit
       | postlude	of unit *)

%start <BurgAST.spec_ast> full

(* %pos int *)
(* %pure *)

(* %eop K_EOF *) 

(* %name Burg *)

%%

full		: ds = decls p1 = PPERCENT rs = rules p2 = PPERCENT	
					{ A.SPEC{head=p1;
						decls=List.rev ds;
						rules=List.rev rs;
						tail=p2} }

decls		: (* empty *)		{ [] }
		| ds = decls d = decl		{ d :: ds }

decl		: K_TERM bs = bindinglist	{ A.TERM (List.rev bs) }
		| K_START id = ID		{ A.START id }
		| K_TERMPREFIX id = ID	{ A.TERMPREFIX id }
		| K_RULEPREFIX id = ID	{ A.RULEPREFIX id }
		| K_SIG id = ID		{ A.SIG id }


bindinglist	: b = binding		{ [b] }
		| bs = bindinglist K_PIPE b = binding
					{ b :: bs }

binding		: id = ID			{ (id, None) }
		| id1 = ID K_EQUAL id2 = ID		{ (id1, Some id2) }

rules		: (* empty *)		{ [] }
		| rs = rules r = rule		{ r :: rs }

rule		: id = ID K_COLON p = pattern K_EQUAL r = rulename c = cost K_SEMICOLON
					{ A.RULE(id, p, r, c) }

rulename	: id = ID			{ id }

pattern		: id = ID 			{ A.PAT(id, []) }
		| id = ID K_LPAREN p = pattern tail = patterntail K_RPAREN		
					{ A.PAT(id, p :: tail) }

patterntail	: (* empty *)		{ [] }
		| K_COMMA p = pattern tail = patterntail
					{ p :: tail }


cost		: (* empty *)		{ [] }
		| K_LPAREN i = INT tail = costtail K_RPAREN	
					{ i :: tail }

costtail	: (* empty *)		{ [] }
		| K_COMMA i = INT tail = costtail	{ i :: tail }

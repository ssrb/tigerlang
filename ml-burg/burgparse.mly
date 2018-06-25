(* burg-gram
**
** Menhir grammar for BURG.
*)
%{

module A = BurgAST

open Core

let outputRaw s = Out_channel.print_endline (s:string)

%}

%token EOF
%token TERM
%token START
%token TERMPREFIX
%token RULEPREFIX
%token SIG
%token COLON
%token SEMICOLON
%token COMMA
%token LPAREN 
%token RPAREN
%token EQUAL
%token PIPE
%token <string list> PPERCENT
%token <int> INT
%token <string> ID 
%token <string list> RAW

%start <BurgAST.spec_ast> full

%%

full : ds =  list(decl) p1 = PPERCENT rs = list(rule) p2 = PPERCENT EOF { 
	A.SPEC { 
		head = p1;
		decls = ds;
		rules = rs;
		tail = p2 
	} 
}

decl : 
	| TERM bs = bindinglist { A.TERM bs }
	| START id = ID { A.START id }
	| TERMPREFIX id = ID { A.TERMPREFIX id }
	| RULEPREFIX id = ID { A.RULEPREFIX id }
	| SIG id = ID	{ A.SIG id }

bindinglist	: bs = separated_nonempty_list(PIPE, binding) { bs }

binding	: 
	| id = ID { (id, None) }
	| id1 = ID EQUAL id2 = ID { (id1, Some id2) }

rule : id = ID COLON p = pattern EQUAL r = rulename c = loption(cost) SEMICOLON { A.RULE(id, p, r, c) }

rulename : id = ID { id }

pattern	: 
	| id = ID { A.PAT(id, []) }
	| id = ID LPAREN ps = separated_nonempty_list(COMMA, pattern) RPAREN { A.PAT(id, ps) }
					
cost : LPAREN cs = separated_nonempty_list(COMMA, INT) RPAREN { cs }

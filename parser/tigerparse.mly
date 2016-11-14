%{
open Core.Std
module A = Absyn
module S = Symbol

type varhelper =  Field of S.symbol * A.pos | Subscript of A.exp * A.pos

(* Little helper to convert from concrete syntax to abstract syntax: auxilliary production rules were introduced to solve a shift/reduce conflict (array init/lvalue) *)
let rec build_lvalue head tails = List.fold tails ~init:head ~f:(fun v t -> 
  match t with 
  | Field(s, p) -> A.FieldVar(v, s, p)
  | Subscript(e, p) -> A.SubscriptVar(v, e, p)
)

(* This will group consecutive type (resp fun) declarations: these are potentially mutually recursive. Similar to ocaml "type ... and ... and ..." (resp let rec ... and
... and ... in ...) *)
let group_recursive_decs decs = List.fold decs ~init:[] ~f:(fun gs d ->
	match gs with
	| g::gs' ->
		(match (g, d) with
		| (A.TypeDec(g'), A.TypeDec(d')) -> A.TypeDec(g' @ d')::gs'
		| (A.FunctionDec(g'), A.FunctionDec(d')) -> A.FunctionDec(g' @ d')::gs'
		| _ -> d::gs)
	| [] -> [d]
) |> List.rev
%}

%token <string> ID
%token <string> STRING
%token <int> INT
%token EOF
%token COMMA 
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK 
%token LBRACE
%token RBRACE
%token DOT 
%token PLUS
%token MINUS
%token MUL
%token DIV
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
%token ASSIGN
%token ARRAY
%token IF
%token THEN
%token ELSE
%token WHILE
%token FOR
%token TO
%token DO
%token LET
%token IN
%token END
%token OF 
%token BREAK
%token NIL
%token FUNCTION
%token VAR
%token TYPE 

%right THEN ELSE DO OF ASSIGN
%left AND OR
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MUL DIV
%left negation
%start <Absyn.exp> prog
%%

prog: e = exp EOF { e }

decs: ds = list(dec) { group_recursive_decs ds }

dec: | TYPE tname = typeid EQ t = ty
     { A.TypeDec([{ name = S.symbol tname; ty = t; pos = $startpos }]) } 
     | VAR vname = ID t = tannot ASSIGN i = exp 
     { A.VarDec({ name = S.symbol vname; escape = ref false; typ = t; init = i; pos = $startpos }) }
     | FUNCTION fname = ID LPAREN p = typefields RPAREN r = tannot EQ b = exp 
     { A.FunctionDec([{ name = S.symbol fname; params = p; result = r; body = b; pos = $startpos }]) }

tannot: a = option(COLON t = typeid { (S.symbol t, $startpos(t)) }) { a }

%inline typeid: s = ID { s }

ty: | tname = typeid { A.NameTy(S.symbol tname, $startpos) }
    | LBRACE fs = typefields RBRACE { A.RecordTy(fs) }
    | ARRAY OF tname = typeid { A.ArrayTy(S.symbol tname, $startpos) }
 
typefield: fname = ID c = COLON tname = typeid { ({ name = S.symbol fname; escape = ref false; typ = S.symbol tname; pos = $startpos(c) } : A.field) }

typefields: fs = separated_list(COMMA, typefield) { fs }

lvalue: | vname = ID { A.SimpleVar(S.symbol vname, $startpos) }
        | s = idsubscriptexp t = list(lvaluetail) { let (i, si, e, se) = s in build_lvalue (A.SubscriptVar( A.SimpleVar(S.symbol i, si), e, se)) t }
        | vname = ID d = DOT fname = ID t = list(lvaluetail) { build_lvalue (A.FieldVar(A.SimpleVar(S.symbol vname, $startpos), S.symbol fname, $startpos(d))) t }

lvaluetail: | DOT fname = ID { Field(S.symbol fname, $startpos) }
            | LBRACK e = exp RBRACK { Subscript(e, $startpos) }

idsubscriptexp: i = ID LBRACK e = exp RBRACK { (i, $startpos(i), e, $startpos(e)) }
%inline op: | PLUS { A.PlusOp }
            | MINUS { A.MinusOp }
            | MUL { A.MulOp }
            | DIV { A.DivOp }
            | EQ { A.EqOp }
            | NEQ { A.NeqOp }
            | LT { A.LtOp }
            | LE { A.LeOp } 
            | GT { A.GtOp }
            | GE { A.GeOp }

seqexp: s = separated_list(SEMICOLON, e = exp { (e, $startpos) }) { A.SeqExp(s) }

exp:  | lval = lvalue { A.VarExp(lval) }
      | NIL  { A.NilExp }
      | LPAREN s = seqexp RPAREN { s }
      | i = INT { A.IntExp(i) }
      | s = STRING { A.StringExp(s, $startpos) }
      | MINUS e = exp %prec negation { OpExp({ left = A.IntExp(0); oper = A.MinusOp; right = e; pos = $startpos }) }
      | fname = ID LPAREN args = separated_list(COMMA, exp) RPAREN  { A.CallExp({ func = S.symbol fname; args = args; pos = $startpos}) }
      | l = exp op = op r = exp { A.OpExp({ left = l; oper = op; right = r; pos = $startpos(op) }) }
      | l = exp op = AND r = exp { A.IfExp({ test = l; then' = r; else' = Some(A.IntExp(0)); pos = $startpos(op) }) }
      | l = exp op = OR r = exp { A.IfExp({ test = l; then' = A.IntExp(1); else' = Some(r); pos = $startpos }) }
      | t = typeid LBRACE fs = separated_list(COMMA, i = ID EQ e = exp { (S.symbol i, e, $startpos) }) RBRACE { A.RecordExp({fields = fs; typ = S.symbol t; pos = $startpos}) }
      | s = idsubscriptexp OF v = exp { let (i, si, s, _) = s in A.ArrayExp({ typ = S.symbol i; size = s; init = v; pos = si }) }
      | v = lvalue a = ASSIGN e = exp { A.AssignExp({ var = v; exp = e; pos = $startpos(a) }) }
      | IF test = exp THEN e = exp { A.IfExp({ test = test; then' = e; else' = None; pos = $startpos }) }
      | IF test = exp THEN e1 = exp ELSE e2 = exp { A.IfExp({ test = test; then' = e1; else' = Some(e2); pos = $startpos }) }
      | WHILE test = exp DO e = exp { A.WhileExp({ test = test; body = e; pos = $startpos }) }
      | FOR vname = ID ASSIGN lo = exp TO hi = exp DO b = exp { A.ForExp({ var = S.symbol(vname); escape = ref false; lo = lo; hi = hi; body = b; pos = $startpos }) }
      | LET d = decs IN s = seqexp END { A.LetExp({ decs = d; body = s; pos = $startpos}) }
      | BREAK { A.BreakExp($startpos) }
